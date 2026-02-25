/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using System.IO;
using System.Linq;
using GDModel;
using GKCore;
using GKCore.Locales;
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
    public FileNewCommand() : base("new_gedcom", LSID.MIFileNew, CommandCategory.File) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        baseContext.Clear();
        PromptHelper.WriteLine("Database created. Records: {0}.", baseContext.Tree.RecordsCount);
    }
}


internal abstract class FileCommand : BaseCommand
{
    public FileCommand(string sign, Enum lsid, CommandCategory category) : base(sign, lsid, category) { }

    protected static void LoadFile(BaseContext baseContext, string selectedFile)
    {
        PromptHelper.WriteLine("Selected file: {0}", selectedFile);
        var result = baseContext.FileLoad(selectedFile, false).GetAwaiter().GetResult();
        PromptHelper.WriteLine("Database loaded successfully. Records: {0}.", baseContext.Tree.RecordsCount);
    }

    protected static void SaveFile(BaseContext baseContext, string selectedFile)
    {
        PromptHelper.WriteLine("Selected file: {0}", selectedFile);
        var result = baseContext.FileSave(selectedFile).GetAwaiter().GetResult();
        PromptHelper.WriteLine("Database saved successfully.");
    }
}


internal class FileLoadCommand : FileCommand
{
    public FileLoadCommand() : base("load_gedcom", LSID.MIFileLoad, CommandCategory.File) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        string selectedFile = PromptHelper.SelectFile(GKUtils.GetAppPath(), ".ged");
        LoadFile(baseContext, selectedFile);
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
    public FileSaveCommand() : base("save_gedcom", LSID.MIFileSave, CommandCategory.File) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        PromptHelper.WriteLine("Not implemented.");
    }
}


internal class FileSaveAsCommand : FileCommand
{
    public FileSaveAsCommand() : base("saveas_gedcom", LSID.MIFileSaveAs, CommandCategory.File) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        string selectedFolder = PromptHelper.SelectFolder(GKUtils.GetAppPath());
        var fileName = Prompt.Input<string>("Enter a new file name (.ged)");
        SaveFile(baseContext, Path.Combine(selectedFolder, fileName + ".ged"));
    }
}


internal class FilePropsCommand : BaseCommand
{
    public FilePropsCommand() : base("properties_gedcom", LSID.MIFileProperties, CommandCategory.File) { }

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
}
