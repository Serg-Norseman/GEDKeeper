/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System.Collections.Generic;
using System.Diagnostics;
using System.Text.Json;
using GDModel;
using GKcli.MCP;
using GKCore;
using GKCore.Options;
using GKCore.Tools;
using GKCore.Utilities;
using GKUI.Platform;

namespace GKcli.Features;

internal class FileNewTool : BaseTool
{
    public FileNewTool() : base("file_new") { }

    public override MCPTool CreateTool()
    {
        return new MCPTool {
            Name = Sign,
            Description = "Create a new empty GEDCOM database",
            InputSchema = MCPToolInputSchema.Empty
        };
    }

    public override List<MCPContent> ExecuteTool(BaseContext baseContext, JsonElement args)
    {
        baseContext.Clear();
        return MCPContent.CreateSimpleContent($"Database created. Records: {baseContext.Tree.RecordsCount}.");
    }
}


internal abstract class FileTool : BaseTool
{
    public FileTool(string sign) : base(sign) { }
}


internal class FileLoadTool : FileTool
{
    public FileLoadTool() : base("file_load") { }

    public override MCPTool CreateTool()
    {
        return new MCPTool {
            Name = Sign,
            Description = "Load a GEDCOM file into the database",
            InputSchema = new MCPToolInputSchema {
                Properties = new Dictionary<string, MCPToolProperty> {
                    ["path"] = new MCPToolProperty { Type = "string", Description = "Path to the .ged file" }
                },
                Required = new List<string> { "path" }
            }
        };
    }

    public override List<MCPContent> ExecuteTool(BaseContext baseContext, JsonElement args)
    {
        string path = MCPHelper.GetRequiredStr(args, "path");

        var sw = Stopwatch.StartNew();
        baseContext.FileLoad(path, false).GetAwaiter().GetResult();
        sw.Stop();

        return MCPContent.CreateSimpleContent($"Database loaded: {path}. Records: {baseContext.Tree.RecordsCount}. Time: {sw.Elapsed.TotalSeconds:F3}s.");
    }
}


internal class FileSaveTool : FileTool
{
    public FileSaveTool() : base("file_saveas") { }

    public override MCPTool CreateTool()
    {
        return new MCPTool {
            Name = Sign,
            Description = "Save the current database to the specified GEDCOM file",
            InputSchema = new MCPToolInputSchema {
                Properties = new Dictionary<string, MCPToolProperty> {
                    ["path"] = new MCPToolProperty { Type = "string", Description = "Path to save the .ged file" }
                },
                Required = new List<string> { "path" }
            }
        };
    }

    public override List<MCPContent> ExecuteTool(BaseContext baseContext, JsonElement args)
    {
        string path = MCPHelper.GetRequiredStr(args, "path");

        var sw = Stopwatch.StartNew();
        baseContext.FileSave(path).GetAwaiter().GetResult();
        sw.Stop();

        return MCPContent.CreateSimpleContent($"Database saved: {path}. Time: {sw.Elapsed.TotalSeconds:F3}s.");
    }
}


internal class FilePropsTool : BaseTool
{
    public FilePropsTool() : base("file_props") { }

    public override MCPTool CreateTool()
    {
        return new MCPTool {
            Name = Sign,
            Description = "Get information about the current database (author, address, record counts)",
            InputSchema = MCPToolInputSchema.Empty
        };
    }

    public override List<MCPContent> ExecuteTool(BaseContext baseContext, JsonElement args)
    {
        var lines = new List<string> { "File properties" };

        GDMSubmitterRecord submitter = baseContext.Tree.GetSubmitter();
        lines.Add($"  Author: {submitter.Name}");
        lines.Add($"  Address: {submitter.Address.Lines.Text}");
        if (submitter.Address.PhoneNumbers.Count > 0)
            lines.Add($"  Telephone: {submitter.Address.PhoneNumbers[0].StringValue}");

        int[] stats = baseContext.Tree.GetRecordStats();
        lines.Add("");
        lines.Add("Record counts:");
        for (int i = 1; i < stats.Length; i++) {
            lines.Add($"  {GKData.RecordTypes[i].Name}: {stats[i]}");
        }

        return MCPContent.CreateSimpleContent(string.Join("\n", lines));
    }
}


internal class FileRecentTool : BaseTool
{
    public FileRecentTool() : base("file_recent") { }

    public override MCPTool CreateTool()
    {
        return new MCPTool {
            Name = Sign,
            Description = "Get a list of recently opened files",
            InputSchema = MCPToolInputSchema.Empty
        };
    }

    public override List<MCPContent> ExecuteTool(BaseContext baseContext, JsonElement args)
    {
        var result = new List<MCPContent>();

        var globOpts = GlobalOptions.Instance;
        for (int i = 0; i < globOpts.MRUFiles.Count; i++) {
            var mruFile = globOpts.MRUFiles[i];
            result.Add(new MCPContent { Text = mruFile.FileName.Replace('\\', '/') });
        }

        return result;
    }
}


internal class FileReloadTool : BaseTool
{
    public FileReloadTool() : base("file_reload") { }

    public override MCPTool CreateTool()
    {
        return new MCPTool {
            Name = Sign,
            Description = "Reload the most recently opened file",
            InputSchema = MCPToolInputSchema.Empty
        };
    }

    public override List<MCPContent> ExecuteTool(BaseContext baseContext, JsonElement args)
    {
        var globOpts = GlobalOptions.Instance;
        if (globOpts.MRUFiles.Count == 0)
            return MCPContent.CreateSimpleContent("No recently opened files available.");

        var mruFile = globOpts.MRUFiles[0];
        string path = mruFile.FileName;

        baseContext.Clear();

        var sw = Stopwatch.StartNew();
        baseContext.FileLoad(path, false).GetAwaiter().GetResult();
        sw.Stop();

        return MCPContent.CreateSimpleContent($"Database reloaded: {path.Replace('\\', '/')}. Records: {baseContext.Tree.RecordsCount}. Time: {sw.Elapsed.TotalSeconds:F3}s.");
    }
}


internal class FileSearchTool : BaseTool
{
    public FileSearchTool() : base("file_search") { }

    public override MCPTool CreateTool()
    {
        return new MCPTool {
            Name = Sign,
            Description = "Find all GEDCOM files on disk in the specified directory",
            InputSchema = new MCPToolInputSchema {
                Properties = new Dictionary<string, MCPToolProperty> {
                    ["path"] = new MCPToolProperty { Type = "string", Description = "Root directory path to search in (e.g., 'd:/')" }
                },
                Required = new List<string> { "path" }
            }
        };
    }

    public override List<MCPContent> ExecuteTool(BaseContext baseContext, JsonElement args)
    {
        string path = MCPHelper.GetRequiredStr(args, "path");

        var result = new List<MCPContent>();

        var gedFiles = SysUtils.FastSearchFiles(path, "*.ged");
        foreach (var fn in gedFiles) {
            result.Add(new MCPContent { Text = fn.Replace('\\', '/') });
        }

        return result;
    }
}


internal class FileValidateTool : BaseTool
{
    public FileValidateTool() : base("file_validate") { }

    public override MCPTool CreateTool()
    {
        return new MCPTool {
            Name = Sign,
            Description = "Check if the current database is valid (not unknown/empty/corrupted)",
            InputSchema = MCPToolInputSchema.Empty
        };
    }

    public override List<MCPContent> ExecuteTool(BaseContext baseContext, JsonElement args)
    {
        bool isUnknown = baseContext.IsUnknown();
        int recordsCount = baseContext.Tree.RecordsCount;

        if (isUnknown && recordsCount == 0) {
            return MCPContent.CreateSimpleContent("Database state: INVALID (unknown, no records). The file is new, empty, or corrupted.");
        }

        var lines = new List<string> {
            $"Database state: VALID",
            $"  Unknown: {isUnknown}",
            $"  Records: {recordsCount}"
        };

        return MCPContent.CreateSimpleContent(string.Join("\n", lines));
    }
}


internal class FileMergeTool : BaseTool
{
    public FileMergeTool() : base("file_merge") { }

    public override MCPTool CreateTool()
    {
        return new MCPTool {
            Name = Sign,
            Description = "Merge another GEDCOM file into the current database",
            InputSchema = new MCPToolInputSchema {
                Properties = new Dictionary<string, MCPToolProperty> {
                    ["path"] = new MCPToolProperty { Type = "string", Description = "Path to the .ged file" }
                },
                Required = new List<string> { "path" }
            }
        };
    }

    public override List<MCPContent> ExecuteTool(BaseContext baseContext, JsonElement args)
    {
        string path = MCPHelper.GetRequiredStr(args, "path");

        var textLog = new TextOutput();

        //var sw = Stopwatch.StartNew();
        TreeTools.MergeTreeFile(baseContext.Tree, path, textLog, true);
        baseContext.SetModified();
        //sw.Stop();
        //return MCPContent.CreateSimpleContent($"Databases merged: {path}. Records: {baseContext.Tree.RecordsCount}. Time: {sw.Elapsed.TotalSeconds:F3}s.");

        return MCPContent.CreateSimpleContent(textLog.ToString());
    }
}
