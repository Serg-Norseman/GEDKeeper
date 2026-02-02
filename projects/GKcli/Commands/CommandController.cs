/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using System.Collections.Generic;
using System.ComponentModel.DataAnnotations;
using System.IO;
using System.Linq;
using GDModel;
using GKCore;
using GKCore.Locales;
using GKUI.Platform;
using Sharprompt;

namespace GKUI.Commands;

internal class CommandController
{
    #region Common

    private readonly Dictionary<string, CommandData> fCommands = new Dictionary<string, CommandData>();

    private static CommandController fInstance;

    public static CommandController Instance
    {
        get {
            if (fInstance == null) {
                fInstance = new CommandController();
            }
            return fInstance;
        }
    }

    private CommandController()
    {
        RegisterCommands(this);
    }

    private void RegisterCommand(string command, CommandCategory category, CommandFunc func)
    {
        var cmd = new CommandData(command, category, func);
        fCommands.Add(command, cmd);
    }

    public void ExecuteCommand(string command, BaseContext baseContext, object obj)
    {
        if (fCommands.TryGetValue(command, out CommandData data)) {
            data.Func(baseContext, obj);
        }
    }

    public IList<string> GetCommands(CommandCategory category, bool hasReturn = false)
    {
        var result = fCommands.Values.Where(c => c.Category == category).Select(c => c.Command).ToList();

        if (hasReturn)
            result.Add(CMD_RETURN);

        return result;
    }

    private static void RegisterCommands(CommandController controller)
    {
        controller.RegisterCommand(CMD_FILE_CAT, CommandCategory.Application, FileMenu);
        controller.RegisterCommand(CMD_FILE_NEW, CommandCategory.File, FileNew);
        controller.RegisterCommand(CMD_FILE_LOAD, CommandCategory.File, FileLoad);
        controller.RegisterCommand(CMD_FILE_LOAD_RECENT, CommandCategory.File, FileLoadRecent);
        controller.RegisterCommand(CMD_FILE_SAVE, CommandCategory.File, FileSave);
        controller.RegisterCommand(CMD_FILE_SAVE_AS, CommandCategory.File, FileSaveAs);
        controller.RegisterCommand(CMD_FILE_PROPS, CommandCategory.File, FileProps);

        controller.RegisterCommand(CMD_INDI_CAT, CommandCategory.Application, IndiMenu);
        controller.RegisterCommand(CMD_INDI_LIST, CommandCategory.Individual, IndiList);
        controller.RegisterCommand(CMD_INDI_ADD, CommandCategory.Individual, IndiAdd);

        controller.RegisterCommand(CMD_FAM_CAT, CommandCategory.Application, FamMenu);
        controller.RegisterCommand(CMD_FAM_LIST, CommandCategory.Family, FamList);

        controller.RegisterCommand(CMD_NOTE_CAT, CommandCategory.Application, NoteMenu);
        controller.RegisterCommand(CMD_NOTE_LIST, CommandCategory.Note, NoteList);
        controller.RegisterCommand(CMD_NOTE_ADD, CommandCategory.Note, NoteAdd);

        controller.RegisterCommand(CMD_MEDIA_CAT, CommandCategory.Application, MediaMenu);
        controller.RegisterCommand(CMD_MEDIA_LIST, CommandCategory.Multimedia, MediaList);

        controller.RegisterCommand(CMD_SOURCE_CAT, CommandCategory.Application, SourceMenu);
        controller.RegisterCommand(CMD_SOURCE_LIST, CommandCategory.Source, SourceList);

        controller.RegisterCommand(CMD_REPO_CAT, CommandCategory.Application, RepositoryMenu);
        controller.RegisterCommand(CMD_REPO_LIST, CommandCategory.Repository, RepositoryList);

        controller.RegisterCommand(CMD_EXIT, CommandCategory.Application, AppExit);
    }

    #endregion

    #region Application operations

    public const string CMD_RETURN = "return";

    public const string CMD_EXIT = "exit";
    public static void AppExit(BaseContext baseContext, object obj)
    {
        if (baseContext.Modified) {
            WriteLine("The file has been modified.");
        }
    }

    #endregion

    #region File operations

    public const string CMD_FILE_CAT = "gedcom files";
    public static void FileMenu(BaseContext baseContext, object obj)
    {
        var cmdList = CommandController.Instance.GetCommands(CommandCategory.File, true);
        var selected = Prompt.Select($"Select a file operation", cmdList);
        if (selected != CMD_RETURN)
            CommandController.Instance.ExecuteCommand(selected, baseContext, obj);
    }

    public const string CMD_FILE_NEW = "new gedcom";
    public static void FileNew(BaseContext baseContext, object obj)
    {
        baseContext.Clear();
        WriteLine("Database created. Records: {0}.", baseContext.Tree.RecordsCount);
    }

    public const string CMD_FILE_LOAD = "load gedcom";
    public static void FileLoad(BaseContext baseContext, object obj)
    {
        string selectedFile = PromptHelper.SelectFile(GKUtils.GetAppPath(), ".ged");
        LoadFile(baseContext, selectedFile);
    }

    public const string CMD_FILE_LOAD_RECENT = "recent gedcom";
    public static void FileLoadRecent(BaseContext baseContext, object obj)
    {
        var files = AppHost.Options.MRUFiles.Select(f => f.FileName).ToList();
        if (files.Count > 0) {
            var selectedFile = Prompt.Select("Select a recent file", files, pageSize: 10);
            LoadFile(baseContext, selectedFile);
        } else {
            WriteLine("No recent files.");
        }
    }

    public const string CMD_FILE_SAVE = "save gedcom";
    public static void FileSave(BaseContext baseContext, object obj)
    {
        //string selectedFile = PromptHelper.SelectFile(GKUtils.GetAppPath(), ".ged");
        //LoadFile(baseContext, selectedFile);

        WriteLine("Not implemented.");
    }

    public const string CMD_FILE_SAVE_AS = "save as gedcom";
    public static void FileSaveAs(BaseContext baseContext, object obj)
    {
        string selectedFolder = PromptHelper.SelectFolder(GKUtils.GetAppPath());
        var fileName = Prompt.Input<string>("Enter a new file name (.ged)");
        SaveFile(baseContext, Path.Combine(selectedFolder, fileName, ".ged"));
    }

    /// <summary>
    /// See <see cref="GKCore.Controllers.FilePropertiesDlgController"/>.
    /// </summary>
    public const string CMD_FILE_PROPS = "properties of gedcom";
    public static void FileProps(BaseContext baseContext, object obj)
    {
        WriteLine("File properties");

        GDMSubmitterRecord submitter = baseContext.Tree.GetSubmitter();
        WriteLine(1, "{0}: [yellow]{1}[/]", LangMan.LS(LSID.GeneralName), submitter.Name);
        WriteLine(1, "{0}: [yellow]{1}[/]", LangMan.LS(LSID.Address), submitter.Address.Lines.Text);
        if (submitter.Address.PhoneNumbers.Count > 0) {
            WriteLine(1, "{0}: [yellow]{1}[/]", LangMan.LS(LSID.Telephone), submitter.Address.PhoneNumbers[0].StringValue);
        }

        WriteLine();
        WriteLine(1, LangMan.LS(LSID.MIFileProperties));
        int[] stats = baseContext.Tree.GetRecordStats();
        for (int i = 1; i < stats.Length; i++) {
            WriteLine(2, "{0}: [yellow]{1}[/]", LangMan.LS(GKData.RecordTypes[i].Name), stats[i]);
        }
    }

    #endregion

    #region Individual operations

    public const string CMD_INDI_CAT = "individuals";
    public static void IndiMenu(BaseContext baseContext, object obj)
    {
        var cmdList = CommandController.Instance.GetCommands(CommandCategory.Individual, true);
        var selected = Prompt.Select($"Select a individual operation", cmdList);
        if (selected != CMD_RETURN)
            CommandController.Instance.ExecuteCommand(selected, baseContext, obj);
    }

    public const string CMD_INDI_LIST = "list individuals";
    public static void IndiList(BaseContext baseContext, object obj)
    {
        var selected = SelectRecord(baseContext, GDMRecordType.rtIndividual, "Select a individual", "Individual: {0}", "No records.");
        if (selected != null)
            IndiChange(selected as GDMIndividualRecord);
    }

    public const string CMD_INDI_ADD = "add individual";
    public static void IndiAdd(BaseContext baseContext, object obj)
    {
        var name = Prompt.Input<string>("Enter the individual's name [first_name /last_name/]");
        var sex = Prompt.Input<char>("Enter the individual's sex [m/f]", validators: [Validators.Required(), SexVal()]);
        var indiRec = baseContext.Tree.CreateIndividual();
        var persName = indiRec.AddPersonalName(new GDMPersonalName());
        persName.ParseString(name);
        indiRec.Sex = (sex == 'm') ? GDMSex.svMale : GDMSex.svFemale;
        WriteLine("Individual: {0}", GKUtils.GetNameString(indiRec, false));

        IndiChange(indiRec);
    }

    private static void IndiChange(GDMIndividualRecord iRec)
    {
        // defaultValue
        var continueFlag = Prompt.Input<bool>("Continue editing? [true/false]");
        if (!continueFlag) return;

        var newEvent = new GDMIndividualEvent();
        CommandForms.InputEvent(newEvent);
    }

    private static Func<object, ValidationResult> SexVal(string errorMessage = null)
    {
        return delegate (object input) {
            if (!(input is char sym)) {
                return ValidationResult.Success;
            }

            sym = char.ToLowerInvariant(sym);
            return (sym == 'm' || sym == 'f') ? ValidationResult.Success : new ValidationResult(errorMessage ?? "");
        };
    }

    #endregion

    #region Family operations

    public const string CMD_FAM_CAT = "families";
    public static void FamMenu(BaseContext baseContext, object obj)
    {
        var cmdList = CommandController.Instance.GetCommands(CommandCategory.Family, true);
        var selected = Prompt.Select($"Select a family operation", cmdList);
        if (selected != CMD_RETURN)
            CommandController.Instance.ExecuteCommand(selected, baseContext, obj);
    }

    public const string CMD_FAM_LIST = "list families";
    public static void FamList(BaseContext baseContext, object obj)
    {
        var selected = SelectRecord(baseContext, GDMRecordType.rtFamily, "Select a family", "Family: {0}", "No records.");
    }

    #endregion

    #region Note operations

    public const string CMD_NOTE_CAT = "notes";
    public static void NoteMenu(BaseContext baseContext, object obj)
    {
        var cmdList = CommandController.Instance.GetCommands(CommandCategory.Note, true);
        var selected = Prompt.Select($"Select a note operation", cmdList);
        if (selected != CMD_RETURN)
            CommandController.Instance.ExecuteCommand(selected, baseContext, obj);
    }

    public const string CMD_NOTE_LIST = "list notes";
    public static void NoteList(BaseContext baseContext, object obj)
    {
        var selected = SelectRecord(baseContext, GDMRecordType.rtNote, "Select a note", "Note: {0}", "No records.");
    }

    public const string CMD_NOTE_ADD = "add note";
    public static void NoteAdd(BaseContext baseContext, object obj)
    {
        var text = Prompt.Input<string>("Enter the note's text");
        WriteLine("Note: {0}", text);
    }

    #endregion

    #region Multimedia operations

    public const string CMD_MEDIA_CAT = "media";
    public static void MediaMenu(BaseContext baseContext, object obj)
    {
        var cmdList = CommandController.Instance.GetCommands(CommandCategory.Multimedia, true);
        var selected = Prompt.Select($"Select a multimedia operation", cmdList);
        if (selected != CMD_RETURN)
            CommandController.Instance.ExecuteCommand(selected, baseContext, obj);
    }

    public const string CMD_MEDIA_LIST = "list multimedia";
    public static void MediaList(BaseContext baseContext, object obj)
    {
        var selected = SelectRecord(baseContext, GDMRecordType.rtMultimedia, "Select a multimedia", "Multimedia: {0}", "No records.");
    }

    #endregion

    #region Source operations

    public const string CMD_SOURCE_CAT = "source";
    public static void SourceMenu(BaseContext baseContext, object obj)
    {
        var cmdList = CommandController.Instance.GetCommands(CommandCategory.Source, true);
        var selected = Prompt.Select($"Select a source operation", cmdList);
        if (selected != CMD_RETURN)
            CommandController.Instance.ExecuteCommand(selected, baseContext, obj);
    }

    public const string CMD_SOURCE_LIST = "list sources";
    public static void SourceList(BaseContext baseContext, object obj)
    {
        var selected = SelectRecord(baseContext, GDMRecordType.rtSource, "Select a source", "Source: {0}", "No records.");
    }

    #endregion

    #region Repository operations

    public const string CMD_REPO_CAT = "repository";
    public static void RepositoryMenu(BaseContext baseContext, object obj)
    {
        var cmdList = CommandController.Instance.GetCommands(CommandCategory.Repository, true);
        var selected = Prompt.Select($"Select a repository operation", cmdList);
        if (selected != CMD_RETURN)
            CommandController.Instance.ExecuteCommand(selected, baseContext, obj);
    }

    public const string CMD_REPO_LIST = "list repositories";
    public static void RepositoryList(BaseContext baseContext, object obj)
    {
        var selected = SelectRecord(baseContext, GDMRecordType.rtRepository, "Select a repository", "Repository: {0}", "No records.");
    }

    #endregion

    #region Utilities

    private static void WriteLine()
    {
        Console.WriteLine();
    }

    private static void WriteLine(string value)
    {
        PromptHelper.WriteMarkupLine(value);
    }

    private static void WriteLine(int indent, string value)
    {
        var strIndent = new string(' ', indent * 2);
        PromptHelper.WriteMarkupLine(strIndent + value);
    }

    private static void WriteLine(string value, params object[] args)
    {
        PromptHelper.WriteMarkupLine(string.Format(value, args));
    }

    private static void WriteLine(int indent, string value, params object[] args)
    {
        var strIndent = new string(' ', indent * 2);
        PromptHelper.WriteMarkupLine(strIndent + string.Format(value, args));
    }

    private static void LoadFile(BaseContext baseContext, string selectedFile)
    {
        WriteLine("Selected file: {0}", selectedFile);
        var result = baseContext.FileLoad(selectedFile, false).GetAwaiter().GetResult();
        WriteLine("Database loaded successfully. Records: {0}.", baseContext.Tree.RecordsCount);
    }

    private static void SaveFile(BaseContext baseContext, string selectedFile)
    {
        WriteLine("Selected file: {0}", selectedFile);
        var result = baseContext.FileSave(selectedFile).GetAwaiter().GetResult();
        WriteLine("Database saved successfully.");
    }

    private static GDMRecord SelectRecord(BaseContext baseContext, GDMRecordType recordType, string prompt, string yesMsg, string noMsg)
    {
        GDMRecord result = null;

        var recList = baseContext.Tree.GetRecords(recordType);
        if (recList.Count > 0) {
            result = Prompt.Select(prompt, recList,
                pageSize: 10,
                textSelector: (GDMRecord r) => { return GKUtils.GetRecordName(baseContext.Tree, r, false); });

            WriteLine(string.Format(yesMsg, GKUtils.GetRecordName(baseContext.Tree, result, false)));
        } else {
            WriteLine(noMsg);
        }

        return result;
    }

    #endregion
}
