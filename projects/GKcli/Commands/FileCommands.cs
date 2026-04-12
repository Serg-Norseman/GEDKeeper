/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Text.Json;
using GDModel;
using GKcli.MCP;
using GKCore;
using GKCore.Locales;
using GKCore.Options;
using GKCore.Utilities;
using GKUI.Platform;
using Sharprompt;

namespace GKcli.Commands;

internal class FileMenuCommand : BaseCommand
{
    public FileMenuCommand() : base("gedcom_files", LSID.MIFile, CommandCategory.Application) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        CommandController.SelectCommand(CommandCategory.File, true, "Select a file operation");
    }
}


internal class FileNewCommand : BaseCommand
{
    public FileNewCommand() : base("file_new", LSID.MIFileNew, CommandCategory.File) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        baseContext.Clear();
        PromptHelper.WriteLine("Database created. Records: {0}.", baseContext.Tree.RecordsCount);
    }

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


internal abstract class FileCommand : BaseCommand
{
    public FileCommand(string sign, Enum lsid, CommandCategory category) : base(sign, lsid, category) { }

    protected static void LoadFile(BaseContext baseContext, string selectedFile)
    {
        PromptHelper.WriteLine("Selected file: {0}", selectedFile);
        var sw = Stopwatch.StartNew();
        var result = baseContext.FileLoad(selectedFile, false).GetAwaiter().GetResult();
        sw.Stop();
        PromptHelper.WriteLine("Database loaded successfully. Records: {0}. Time: {1:F3}s.", baseContext.Tree.RecordsCount, sw.Elapsed.TotalSeconds);
    }

    protected static void SaveFile(BaseContext baseContext, string selectedFile)
    {
        PromptHelper.WriteLine("Selected file: {0}", selectedFile);
        var sw = Stopwatch.StartNew();
        var result = baseContext.FileSave(selectedFile).GetAwaiter().GetResult();
        sw.Stop();
        PromptHelper.WriteLine("Database saved successfully. Time: {0:F3}s.", sw.Elapsed.TotalSeconds);
    }
}


internal class FileLoadCommand : FileCommand
{
    public FileLoadCommand() : base("file_load", LSID.MIFileLoad, CommandCategory.File) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        string selectedFile = PromptHelper.SelectFile(GKUtils.GetAppPath(), ".ged");
        LoadFile(baseContext, selectedFile);
    }

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
        string path = MCPHelper.GetRequiredArgument(args, "path");

        var sw = Stopwatch.StartNew();
        baseContext.FileLoad(path, false).GetAwaiter().GetResult();
        sw.Stop();

        return MCPContent.CreateSimpleContent($"Database loaded: {path}. Records: {baseContext.Tree.RecordsCount}. Time: {sw.Elapsed.TotalSeconds:F3}s.");
    }
}


internal class FileLoadRecentCommand : FileCommand
{
    public FileLoadRecentCommand() : base("recent_gedcom", LSID.MIMRUFiles, CommandCategory.File) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        var files = AppHost.Options.MRUFiles.Select(f => f.FileName).ToList();
        if (files.Count > 0) {
            var selectedFile = Prompt.Select("Select a recent file", files, pageSize: 10);
            LoadFile(baseContext, selectedFile);
        } else {
            PromptHelper.WriteLine("No recent files.");
        }
    }
}


internal class FileSaveCommand : BaseCommand
{
    public FileSaveCommand() : base("file_save", LSID.MIFileSave, CommandCategory.File) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        PromptHelper.WriteLine("Not implemented.");
    }
}


internal class FileSaveAsCommand : FileCommand
{
    public FileSaveAsCommand() : base("file_saveas", LSID.MIFileSaveAs, CommandCategory.File) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        string selectedFolder = PromptHelper.SelectFolder(GKUtils.GetAppPath());
        var fileName = Prompt.Input<string>("Enter a new file name (.ged)");
        SaveFile(baseContext, Path.Combine(selectedFolder, fileName + ".ged"));
    }

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
        string path = MCPHelper.GetRequiredArgument(args, "path");

        var sw = Stopwatch.StartNew();
        baseContext.FileSave(path).GetAwaiter().GetResult();
        sw.Stop();

        return MCPContent.CreateSimpleContent($"Database saved: {path}. Time: {sw.Elapsed.TotalSeconds:F3}s.");
    }
}


internal class FilePropsCommand : BaseCommand
{
    public FilePropsCommand() : base("file_props", LSID.MIFileProperties, CommandCategory.File) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        PromptHelper.WriteLine("File properties");

        GDMSubmitterRecord submitter = baseContext.Tree.GetSubmitter();
        PromptHelper.WriteLine(1, "{0}: [yellow]{1}[/]", LangMan.LS(LSID.Author), submitter.Name);
        PromptHelper.WriteLine(1, "{0}: [yellow]{1}[/]", LangMan.LS(LSID.Address), submitter.Address.Lines.Text);
        if (submitter.Address.PhoneNumbers.Count > 0) {
            PromptHelper.WriteLine(1, "{0}: [yellow]{1}[/]", LangMan.LS(LSID.Telephone), submitter.Address.PhoneNumbers[0].StringValue);
        }

        PromptHelper.WriteLine();
        PromptHelper.WriteLine(1, LangMan.LS(LSID.MIFileProperties));
        int[] stats = baseContext.Tree.GetRecordStats();
        for (int i = 1; i < stats.Length; i++) {
            PromptHelper.WriteLine(2, "{0}: [yellow]{1}[/]", LangMan.LS(GKData.RecordTypes[i].Name), stats[i]);
        }
    }

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

internal class FileRecentCommand : BaseCommand
{
    public FileRecentCommand() : base("file_recent", null, CommandCategory.File) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Empty for interactive mode
    }

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


internal class FileReloadCommand : BaseCommand
{
    public FileReloadCommand() : base("file_reload", null, CommandCategory.File) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Empty for interactive mode
    }

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


internal class FileSearchCommand : BaseCommand
{
    public FileSearchCommand() : base("file_search", null, CommandCategory.File) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Empty for interactive mode
    }

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
        string path = MCPHelper.GetRequiredArgument(args, "path");

        var result = new List<MCPContent>();

        var gedFiles = SysUtils.FastSearchFiles(path, "*.ged");
        foreach (var fn in gedFiles) {
            result.Add(new MCPContent { Text = fn.Replace('\\', '/') });
        }

        return result;
    }
}


internal class FileValidateCommand : BaseCommand
{
    public FileValidateCommand() : base("file_validate", null, CommandCategory.File) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Empty for interactive mode
    }

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
