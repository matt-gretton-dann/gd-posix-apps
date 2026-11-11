// SPDX-License-Identifier: Apache-2.0
using System.Text.Json;
using Microsoft.Diagnostics.Tracing;

if (args.Length != 3)
    throw new ArgumentException("expected trace, checked child PID list, and report paths");

var pids = JsonSerializer.Deserialize<HashSet<int>>(File.ReadAllText(args[1]))
    ?? throw new InvalidDataException("missing child PID list");
var images = new Dictionary<int, List<(string Name, ulong Base, ulong Size)>>();
var samples = new List<(int Pid, ulong Ip)>();
using var source = new ETWTraceEventSource(args[0]);
source.Kernel.ImageGroup += data =>
{
    if (!pids.Contains(data.ProcessID))
        return;
    if (!images.TryGetValue(data.ProcessID, out var loaded))
        images.Add(data.ProcessID, loaded = new());
    var name = Path.GetFileName(data.FileName);
    if (!string.IsNullOrEmpty(name) && data.ImageSize > 0)
        loaded.Add((name.ToLowerInvariant(), data.ImageBase, (ulong)data.ImageSize));
};
source.Kernel.PerfInfoSample += data =>
{
    if (pids.Contains(data.ProcessID))
        samples.Add((data.ProcessID, data.InstructionPointer));
};
source.Process();

var modules = new SortedDictionary<string, int>();
var offsets = new SortedDictionary<ulong, int>();
foreach (var (pid, ip) in samples)
{
    var image = images.GetValueOrDefault(pid)?.Find(i => ip >= i.Base && ip - i.Base < i.Size);
    var name = image?.Name ?? "unmapped";
    modules[name] = modules.GetValueOrDefault(name) + 1;
    if (name == "bc.exe" && image is { } program)
    {
        var offset = ip - program.Base;
        offsets[offset] = offsets.GetValueOrDefault(offset) + 1;
    }
}

var report = new
{
    traceEventsLost = source.EventsLost,
    checkedPids = pids.Count,
    sampledPids = samples.Select(s => s.Pid).Distinct().Count(),
    sampleCount = samples.Count,
    modules,
    executableRva = offsets,
};
File.WriteAllText(args[2], JsonSerializer.Serialize(report, new JsonSerializerOptions { WriteIndented = true }));
return report.traceEventsLost == 0 && offsets.Count > 0 ? 0 : 1;
