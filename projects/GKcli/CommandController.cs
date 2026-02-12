/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using System.Collections.Generic;
using System.Linq;
using GDModel;
using GKcli.Commands;
using GKCore;
using GKUI.Platform;
using Sharprompt;

namespace GKcli;

internal class CommandController
{
    #region Common

    private static readonly BaseContext fBaseContext = new BaseContext(null);
    private static readonly Dictionary<string, BaseCommand> fCommands = new Dictionary<string, BaseCommand>();
    private static Dictionary<string, object> fVariables = new Dictionary<string, object>();

    static CommandController()
    {
        // Files operations
        RegisterCommand(new FileMenuCommand());
        RegisterCommand(new FileNewCommand());
        RegisterCommand(new FileLoadCommand());
        RegisterCommand(new FileLoadRecentCommand());
        RegisterCommand(new FileSaveCommand());
        RegisterCommand(new FileSaveAsCommand());
        RegisterCommand(new FilePropsCommand());

        // Events
        RegisterCommand(new EventEditCommand());

        // Individuals operations
        RegisterCommand(new IndiMenuCommand());
        RegisterCommand(new IndiListCommand());
        RegisterCommand(new IndiAddCommand());

        // Families operations
        RegisterCommand(new FamMenuCommand());
        RegisterCommand(new FamListCommand());

        // Notes operations
        RegisterCommand(new NoteMenuCommand());
        RegisterCommand(new NoteListCommand());
        RegisterCommand(new NoteAddCommand());
        RegisterCommand(new NoteEditCommand());
        RegisterCommand(new NoteDeleteCommand());

        // Multimedia operations
        RegisterCommand(new MediaMenuCommand());
        RegisterCommand(new MediaListCommand());

        // Sources operations
        RegisterCommand(new SourceMenuCommand());
        RegisterCommand(new SourceListCommand());

        // Repositories operations
        RegisterCommand(new RepositoryMenuCommand());
        RegisterCommand(new RepositoryListCommand());

        // Service
        RegisterCommand(new ServiceMenuCommand());
        RegisterCommand(new ToolsMenuCommand());
        RegisterCommand(new OptionsCommand());
        RegisterCommand(new LangChangeCommand());

        // Tools
        RegisterCommand(new TreeCompareCommand());
        RegisterCommand(new TreeMergeCommand());
        RegisterCommand(new TreeSplitCommand());
        RegisterCommand(new RecMergeCommand());
        RegisterCommand(new FamilyGroupsCommand());
        RegisterCommand(new TreeCheckCommand());
        RegisterCommand(new PatSearchCommand());
        RegisterCommand(new PlacesManagerCommand());

        // Application and menu
        RegisterCommand(new MenuReturnCommand());
        RegisterCommand(new AppExitCommand());
    }

    private static void RegisterCommand(BaseCommand commandInstance)
    {
        fCommands.Add(commandInstance.Sign, commandInstance);
    }

    public static void ExecuteCommand(string sign, BaseContext baseContext, object obj)
    {
        if (fCommands.TryGetValue(sign, out BaseCommand cmd)) {
            cmd.Execute(baseContext, obj);
        }
    }

    public static IList<BaseCommand> GetCommands(CommandCategory category, bool hasReturn = false)
    {
        var result = fCommands.Values.Where(c => c.Category == category).ToList();

        if (hasReturn)
            result.Add(new MenuReturnCommand());

        return result;
    }

    public const string CMD_RETURN = "return";
    public const string CMD_EXIT = "exit";

    public static string SelectCommand(CommandCategory category, bool hasReturn, string message)
    {
        var cmdList = GetCommands(category, hasReturn);

        Console.WriteLine();
        var selected = Prompt.Select(message, cmdList,
            textSelector: (BaseCommand cmd) => { return cmd.Text; });

        if (selected != null) {
            ExecuteCommand(selected.Sign, fBaseContext, null);
            return selected.Sign;
        } else {
            return string.Empty;
        }
    }

    #endregion

    #region Utilities

    internal static bool GetConfirm(string message)
    {
        return PromptHelper.GetConfirm(message, 'Д', 'н', "Пожалуйста, введите '{0}' или '{1}'");
    }

    internal static void LoadFile(BaseContext baseContext, string selectedFile)
    {
        PromptHelper.WriteLine("Selected file: {0}", selectedFile);
        var result = baseContext.FileLoad(selectedFile, false).GetAwaiter().GetResult();
        PromptHelper.WriteLine("Database loaded successfully. Records: {0}.", baseContext.Tree.RecordsCount);
    }

    internal static void SaveFile(BaseContext baseContext, string selectedFile)
    {
        PromptHelper.WriteLine("Selected file: {0}", selectedFile);
        var result = baseContext.FileSave(selectedFile).GetAwaiter().GetResult();
        PromptHelper.WriteLine("Database saved successfully.");
    }

    internal static GDMRecord SelectRecord(BaseContext baseContext, GDMRecordType recordType, string prompt, string yesMsg, string noMsg)
    {
        GDMRecord result = null;

        var recList = baseContext.Tree.GetRecords(recordType);
        if (recList.Count > 0) {
            result = Prompt.Select(prompt, recList,
                pageSize: 10,
                textSelector: (GDMRecord r) => { return GKUtils.GetRecordName(baseContext.Tree, r, false); });

            PromptHelper.WriteLine(string.Format(yesMsg, GKUtils.GetRecordName(baseContext.Tree, result, false)));
        } else {
            PromptHelper.WriteLine(noMsg);
        }

        return result;
    }

    public static void DeleteRecord<T>(BaseContext baseContext, string expectedMsg) where T : GDMRecord
    {
        var rec = GetVariable<GDMNoteRecord>("selectedObj");
        if (rec == null) {
            PromptHelper.WriteLine(expectedMsg);
            return;
        }

        bool result = GetConfirm("Подтвердите удаление");
        if (result) {
            baseContext.DeleteRecord(rec);
            SetVariable("selectedObj", null);
        }
    }

    #endregion

    #region Variables

    public static void SetVariable(string varName, object varValue)
    {
        fVariables[varName] = varValue;
    }

    public static T GetVariable<T>(string varName) where T : class
    {
        fVariables.TryGetValue(varName, out object result);
        return result as T;
    }

    #endregion
}
