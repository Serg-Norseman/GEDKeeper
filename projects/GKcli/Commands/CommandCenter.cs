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
using System.Linq;
using GDModel;
using GKCore;
using GKUI.Platform;
using Sharprompt;

namespace GKUI.Commands;

internal class CommandController
{
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
        CommandCenter.RegisterCommands(this);
    }

    public void RegisterCommand(string command, CommandCategory category, CommandFunc func)
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
            result.Add(CommandCenter.CMD_RETURN);

        return result;
    }
}

internal static class CommandCenter
{
    public const string CMD_RETURN = "return";

    public static void RegisterCommands(CommandController controller)
    {
        controller.RegisterCommand(CMD_FILE_CAT, CommandCategory.Application, FileMenu);
        controller.RegisterCommand(CMD_FILE_LOAD, CommandCategory.File, FileLoad);

        controller.RegisterCommand(CMD_INDI_CAT, CommandCategory.Application, IndiMenu);
        controller.RegisterCommand(CMD_INDI_LIST, CommandCategory.Individual, IndiList);
        controller.RegisterCommand(CMD_INDI_ADD, CommandCategory.Individual, IndiAdd);

        controller.RegisterCommand(CMD_NOTE_CAT, CommandCategory.Application, NoteMenu);
        controller.RegisterCommand(CMD_NOTE_ADD, CommandCategory.Note, NoteAdd);

        controller.RegisterCommand(CMD_EXIT, CommandCategory.Application, AppExit);
    }

    #region Application operations

    public const string CMD_EXIT = "exit";

    public static void AppExit(BaseContext baseContext, object obj)
    {
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

    public const string CMD_FILE_LOAD = "load gedcom";

    public static void FileLoad(BaseContext baseContext, object obj)
    {
        string selectedFile = PromptHelper.SelectFile(GKUtils.GetAppPath(), ".ged");
        Console.WriteLine($"Selected file: {selectedFile}");
        var result = baseContext.FileLoad(selectedFile, false).GetAwaiter().GetResult();
        Console.WriteLine($"Database loaded successfully. Records: {baseContext.Tree.RecordsCount}.");
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
        var indiList = baseContext.Tree.GetRecords(GDMRecordType.rtIndividual).Select(i => GKUtils.GetNameString((GDMIndividualRecord)i, false)).ToList();
        var selected = Prompt.Select($"Select a individual", indiList, pageSize: 10);
        Console.WriteLine($"Individual: {selected}");
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
        Console.WriteLine($"Individual: {GKUtils.GetNameString(indiRec, false)}");
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

    #region Note operations

    public const string CMD_NOTE_CAT = "notes";
    public static void NoteMenu(BaseContext baseContext, object obj)
    {
        var cmdList = CommandController.Instance.GetCommands(CommandCategory.Note, true);
        var selected = Prompt.Select($"Select a note operation", cmdList);
        if (selected != CMD_RETURN)
            CommandController.Instance.ExecuteCommand(selected, baseContext, obj);
    }

    public const string CMD_NOTE_ADD = "add note";
    public static void NoteAdd(BaseContext baseContext, object obj)
    {
        var text = Prompt.Input<string>("Enter the note's text");
        Console.WriteLine($"Note: {text}");
    }

    #endregion
}
