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

    public const string CMD_RETURN = "return";
    public const string CMD_EXIT = "exit";

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

    public static string SelectCommand(CommandCategory category, bool hasReturn, string message)
    {
        var cmdList = fCommands.Values.Where(c => c.Category == category).ToList();
        if (hasReturn)
            cmdList.Add(new MenuReturnCommand());

        Console.WriteLine();
        var selected = Prompt.Select(message, cmdList,
            textSelector: (BaseCommand cmd) => { return cmd.Text; });

        if (selected != null) {
            if (fCommands.TryGetValue(selected.Sign, out BaseCommand cmd)) {
                cmd.Execute(fBaseContext, null);
            }

            return selected.Sign;
        } else {
            return string.Empty;
        }
    }

    internal static bool GetConfirm(string message)
    {
        string answers = CLILangMan.LS(CLS.Answers);
        if (string.IsNullOrEmpty(answers) || answers.Length != 2) answers = "Yn";

        return PromptHelper.GetConfirm(message, answers[0], answers[1], CLILangMan.LS(CLS.ConfirmError));
    }

    internal static void SetVariable(string varName, object varValue)
    {
        fVariables[varName] = varValue;
    }

    internal static T GetVariable<T>(string varName) where T : class
    {
        fVariables.TryGetValue(varName, out object result);
        return result as T;
    }

    #endregion
}
