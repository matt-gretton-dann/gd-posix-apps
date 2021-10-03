# /usr/bin/env python3
"""Generate Messages files from JSON Input

Usage: generate-messages.py [options] <base-file>

Where <base-file> is the base messages JSON file.

Options describe the type of output:

 * --addition-file <file>: Add an additional JSON file to consider (TBD)
 * --json <file>: If --locale is specified output the message file for the
   given locale.  If --locale is not specified output a merged message file.
 * --header <file>: Output a C/C++ header file for messages.
 * --cat <file>: If --locale is speicified, output the catalogue file for the
   given locale.  If --locale is not specified output a file for all locales,
   in the directory given by --cat, uses --cat-id to generated the locale root
   name.  In this case the list of catalogues generated will be printed to
   stdout.
 * --msg <file>: If --locale is speicified, output the message file for the
   given locale.  If --locale is not specified output a file for all locales,
   in the directory given by --cat, uses --cat-id to generated the locale root
   name.  In this case the list of message catalogues generated will be printed
   to stdout.
 * --locale <locale>: Locale
 * --cat-id <id>: Root name for catalogue file, used by --header.
"""

import argparse
import filecmp
import hashlib
import json
import pathlib
import os
import re
import shutil
import subprocess
import sys


def validate_locale(locale):
    """Validate whether locale matches the acceptable pattern."""
    r = re.compile(r"[a-z][a-z](_[A-Z][A-Z])?(\.[A-Za-z0-9]+)?$")
    if locale == 'C' or r.match(locale):
        return True
    else:
        return False


def lang(locale):
    """Return the language (first) part of a locale."""
    if locale == 'C':
        return 'C'
    else:
        return locale[:2]


def open_maybe_stdout(fname):
    """Open file for writing.

    If fname is '-' then we use stdout.
    """
    if fname == "w":
        return sys.stdout
    else:
        return open(fname, "w")


class Messages:
    """Messages structure."""

    def __init__(self, base_file):
        """Construct a messages structure from a base JSON input file.

        The specified input file must match the /schema/messages.schema.json
        schema, and provide at least a "C" locale message for all messages.

        This will raise a RuntimeError if the contents of the base file are
        not valid.
        """
        with open(base_file) as fp:
            self._data = json.load(fp)
        self._validate()

    def _check_identifier(self, d, pk, k):
        """Check that an identifier is valid.  Raising errors if not.

        This will check that the key k is in the dictionary d.  Also, if pk
        is not None then it will also check that d[k] is unique.
        """
        if k not in d:
            raise RuntimeError(f"No '{d}' identifier in dict {d}")
        if pk is not None:
            if k in pk:
                raise RuntimeError(
                    f"Identifier '{d}' with value {k} has been repeated.")

    def _check_locale(self, l):
        """Raise a runtime error if the given locale is not valid."""
        if not validate_locale(l):
            raise RuntimeError(f"Locale {l} is not valid in input files.")

    def _validate(self):
        """Validate our contents."""
        sets = []
        for s in self._data:
            self._check_identifier(s, sets, 'set')
            self._check_identifier(s, sets, 'id')
            self._check_identifier(s, None, 'messages')
            sets.extend([s['id'], s['set']])

            for m in s['messages']:
                msgs = []
                self._check_identifier(m, msgs, 'msg')
                self._check_identifier(m, msgs, 'id')
                self._check_identifier(m, None, 'description')
                self._check_identifier(m, None, 'C')
                msgs.extend([m['id'], m['msg']])

                for k in m.keys():
                    if k not in ['msg', 'id', 'description', 'C']:
                        self._check_locale(k)

    def add_file(self, additional_file):
        """Merge in contents of additional_file.

        Not yet implemented.
        """
        self._validate()
        raise RuntimeError("--additional_file NOT IMPLEMENTED YET\n")

    def output_merged_json(self, json_fp):
        """Output the JSON of all input to the given file."""
        json.dump(self._data, json_fp, ensure_ascii=True, indent=2)

    def output_json(self, json_fp, locale):
        """Output JSON for a particular locale to the given file.

        This outputs enough JSON to see the messages that will be used for a
        given locale.  This can then be updated and used as the input for a
        particular locale.

        Unless the locale is "C" the output file is not suitable as the base
        input JSON file.
        """
        data = []
        for s in self._data:
            s2 = {k: v for k, v in s.items() if k in ['set', 'id']}
            s2['messages'] = []
            for m in s['messages']:
                m2 = {k: v for k, v in m.items() if k in [
                    'msg', 'id', 'description', self._get_locale_key(m, locale)]}
                s2['messages'].append(m2)
            data.append(s2)

        json.dump(data, json_fp, ensure_ascii=True, indent=2)

    def _get_locale_key(self, m, locale):
        """Get the key to use to get the message for locale out of m."""
        if locale in m:
            return locale
        elif lang(locale) in m:
            return lang(locale)
        else:
            return 'C'

    def _find_msg_for_locale(self, m, locale):
        """Get the message for the given locale out of m."""
        return m[self._get_locale_key(m, locale)] \
            .encode('unicode-escape') \
            .decode('ASCII')

    def output_header(self, fp, header_file, catalogue_id):
        """Output a C/C++ header to the given file handle.

        Named arguments:
        fp -- File handle.
        header_file -- Name of the header file
        catalogue_id -- Catalogue ID in use.
        """
        set_ids = []
        msg_ids = []
        default = None
        msg_strs = ""
        set_strs = dict()

        for s in self._data:
            if default is None:
                default = s['set']
            set_ids.append(f"\n  {s['set']} = {s['id']},")
            set_str_var = f"{s['set']}_strs_".lower()
            set_strs[s['id'] - 1] = set_str_var
            strs = dict()
            for m in s['messages']:
                msg_ids.append(f"\n  {m['msg']} = {m['id']},"),
                strs[m['id'] - 1] = self._find_msg_for_locale(m, "C")
            max_s = max(strs.keys()) + 1
            msg_strs += \
                f"  static constexpr std::array<char const*, {max_s}> {set_str_var} = {{\n"
            for i in range(max_s):
                if i in strs:
                    msg_strs += f"    \"{strs[i]}\",\n"
                else:
                    msg_strs += f"    nullptr,\n"
            msg_strs += f"  }};\n\n"

        max_s = max(set_strs.keys()) + 1
        set_str_array = ""
        for i in range(max_s):
            msgset = set_strs.get(i, 'empty_')
            set_str_array += f"\n    GD::Std::span({msgset}),"

        hash_object = hashlib.md5(header_file.encode())
        include_guard = f"_{hash_object.hexdigest()}_INCLUDED".upper()
        print(rf"""
/** \file  {header_file}
 *  \brief Messages header file.
 *
 * Generated from:
 * {' '.join(sys.argv)}
 */

#ifndef {include_guard}
#define {include_guard}

#include "util/messages.hh"

#include <array>
#include "gd/span.hh"

namespace GD::{catalogue_id.title()}
{{
enum class Set
{{{"".join(set_ids)}
}};

enum class Msg
{{{"".join(msg_ids)}
}};

struct MessageData
{{
private:
{msg_strs}
  static constexpr std::array<char const*, 0> empty_ = {{}};
public:
  using SetEnum = Set;
  using MessageEnum = Msg;

  static constexpr char const * catalogue_ = "{catalogue_id.lower()}";
  static SetEnum const default_set_ = Set:: {default};
  static constexpr std::array<GD::Std::span<char const* const>, {max_s}> messages_ = {{{set_str_array}
  }};
}};

using Messages = GD::Messages<MessageData>;

}} // namespace GD::{catalogue_id.title()}

#endif // {include_guard}
""",
              file=fp)

    def _get_cat(self, locale):
        """Get the core part of the .cat file for a given locale."""
        result = ""
        for s in self._data:
            result += f"\n$set {s['id']}\n"
            for m in s['messages']:
                result += f"{m['id']} {self._find_msg_for_locale(m, locale)}\n"
        return result

    def _get_locales(self):
        """Get a list of all locales with at least one message."""
        locales = dict()
        for s in self._data:
            for m in s['messages']:
                for k in m.keys():
                    if k not in ['msg', 'id', 'description']:
                        locales[k] = True
        return locales.keys()

    def output_cat(self, fp, locale):
        """Output the catalogue for the given locale to the file handle."""
        fp.write(f"$ Generated from: {' '.join(sys.argv)}\n")
        fp.write(f"$ For locale: {locale}\n")
        fp.write(self._get_cat(locale))

    def output_msg(self, fname, locale, gencat_exe):
        """Out the message file for the given locale to fname using gencat_exe.
        """
        cat = self._get_cat(locale)
        if fname != "-" and os.path.exists(fname):
            os.remove(fname)
        subprocess.run([gencat_exe, fname, "-"], input=cat,
                       check=True, universal_newlines=True)

    def _update_file(self, dest, src):
        """Copy src to dest but only if there is an update."""
        if not os.path.isfile(dest) or not filecmp.cmp(dest, src, shallow=False):
            shutil.copy2(src=src, dst=dest)
        os.remove(src)

    def _output_locale_files(self, root, cat_id, extension, action):
        """Iterate over the locales calling action on each one.

        Named arguments:
        root -- Root of path for file outputs.
        cat_id -- Catalogue ID
        extension -- Extension for files that will be output.
        action -- function to generate output file.  Will be called as
        action(file, locale)

        Will generate files (in UNIX standard):
        /root/cat_id.locale.extension - Output files, only updated if necessary
        /root/cat_id.extension.stamp - Stamp saying we have been successful
        /root/cat_id.locale.extension.temp - Temporary file, removed after use.
        """
        def output_file(locale):
            if locale == "C":
                f = os.path.join(root, f"{cat_id}.{extension}")
                tf = os.path.join(root, f"{cat_id}.{extension}.temp")
            else:
                f = os.path.join(root, f"{cat_id}.{locale}.{extension}")
                tf = os.path.join(root, f"{cat_id}.{locale}.{extension}.temp")
            print(f"{f}")
            action(tf, locale)
            self._update_file(dest=f, src=tf)

        for locale in self._get_locales():
            output_file(locale)

        stamp = pathlib.Path(os.path.join(root, f"{cat_id}.{extension}.stamp"))
        stamp.touch(exist_ok=True)

    def output_cats(self, root, cat_id):
        """Output the catalogues for all locales.

        Parameters:
        root -- Root directory to output to.
        cat_id -- Catalogue ID.

        Produces files for form:

        root/cat_id.{locale}.cat - Catalogue for locale.
        root/cat_id.cat - Catalogue for C locale.
        root/cat_id.cat.stamp - Timestamped file for dependency tracking.
        """
        def output_cat(f, locale):
            with open(f, "w") as fp:
                self.output_cat(fp, locale)

        self._output_locale_files(root, cat_id, "cat", output_cat)

    def output_msgs(self, root, cat_id, gencat_exe):
        """Output the compiled message catalogues for all locales.

        Parameters:
        root -- Root directory to output to.
        cat_id -- Catalogue ID.
        gencat_exe -- Path to gencat executable.

        Produces files for form:

        root/cat_id.{locale}.msg - Messages for locale.
        root/cat_id.msg - Messages for C locale.
        root/cat_id.msg.stamp - Timestamped file for dependency tracking.
        """
        self._output_locale_files(
            root, cat_id, "msg",
            lambda f, locale: self.output_msg(f, locale, gencat_exe))


# Argument parsing
ap = argparse.ArgumentParser()
ap.add_argument("base_file", help="Base JSON input file")
ap.add_argument("--additional-file", action='append',
                help="Additional JSON input files.")
ap.add_argument("--json", help="Output file for merged JSON.")
ap.add_argument("--header", help="Output a C/C++ header file for messages.")
ap.add_argument("--cat", help="Output a message catalogue file.")
ap.add_argument("--msg", help="Output a compiled message catalogue file.")
ap.add_argument(
    "--cat-id", help="Root name of catalogue file, used by --header.")
ap.add_argument("--locale", help="Locale to output data from")
ap.add_argument("--gencat", help="path to gencat executable.")
args = ap.parse_args()

# Validate command line arguments.
if args.locale is not None and not validate_locale(args.locale):
    raise RuntimeError(f"Locale {args.locale} is not valid.\n")

if args.gencat is not None and not os.path.exists(args.gencat):
    raise RuntimeError(
        f"Executable passed to --gencat does not exist: {args.gencat}")

# Read messages
msgs = Messages(args.base_file)
if args.additional_file is not None:
    for additional_file in args.additional_file:
        msgs.add_file(additional_file)


# Output JSON if requested.
if args.json is not None:
    with open_maybe_stdout(args.json) as fp:
        if args.locale is None:
            msgs.output_merged_json(fp)
        else:
            msgs.output_json(fp, args.locale)

# Header file generation
if args.header is not None:
    if args.cat_id is None:
        raise RuntimeError(
            f"{os.path.basename(sys.argv[0])}: --header must be specified with --cat-id.\n")
    with open_maybe_stdout(args.header) as fp:
        msgs.output_header(
            fp,
            "standard_output" if args.header == "-" else args.header,
            args.cat_id)

# Catalogue generation
if args.cat is not None:
    if args.locale is not None:
        with open_maybe_stdout(args.cat) as fp:
            msgs.output_cat(fp, args.locale)
    elif args.cat_id is not None:
        msgs.output_cats(args.cat, args.cat_id)
    else:
        raise RuntimeError(
            "--cat must be specified with either --locale or --cat-id")

# Message generation
if args.msg is not None:
    if args.gencat is None:
        raise RuntimeError("--msg must also have --gencat specified")

    if args.locale is not None:
        msgs.output_msg(args.msg, args.locale, args.gencat)
    elif args.cat_id is not None:
        if args.msg == "-":
            raise RuntimeError("Output to stdout is not valid with --cat-id")
        msgs.output_msgs(args.msg, args.cat_id, args.gencat)
    else:
        raise RuntimeError(
            "--cat must be specified with either --locale or --cat-id")
