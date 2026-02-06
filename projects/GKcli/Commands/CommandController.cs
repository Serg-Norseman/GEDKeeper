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
using GKCore;
using GKUI.Platform;
using Sharprompt;

namespace GKUI.Commands;

internal class CommandController
{
    #region Common

    private readonly Dictionary<string, BaseCommand> fCommands = new Dictionary<string, BaseCommand>();

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
        // Files operations
        RegisterCommand(new FileMenuCommand());
        RegisterCommand(new FileNewCommand());
        RegisterCommand(new FileLoadCommand());
        RegisterCommand(new FileLoadRecentCommand());
        RegisterCommand(new FileSaveCommand());
        RegisterCommand(new FileSaveAsCommand());
        RegisterCommand(new FilePropsCommand());

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

    private void RegisterCommand(BaseCommand commandInstance)
    {
        fCommands.Add(commandInstance.Sign, commandInstance);
    }

    public void ExecuteCommand(string sign, BaseContext baseContext, object obj)
    {
        if (fCommands.TryGetValue(sign, out BaseCommand cmd)) {
            cmd.Execute(baseContext, obj);
        }
    }

    public IList<BaseCommand> GetCommands(CommandCategory category, bool hasReturn = false)
    {
        var result = fCommands.Values.Where(c => c.Category == category).ToList();

        if (hasReturn)
            result.Add(new MenuReturnCommand());

        return result;
    }

    public const string CMD_RETURN = "return";
    public const string CMD_EXIT = "exit";

    public string SelectCommand(CommandCategory category, bool hasReturn, string message, BaseContext baseContext)
    {
        var cmdList = GetCommands(category, hasReturn);

        var selected = Prompt.Select(message, cmdList,
            textSelector: (BaseCommand cmd) => { return cmd.Text; });

        if (selected != null && selected.Sign != CommandController.CMD_RETURN) {
            ExecuteCommand(selected.Sign, baseContext, null);
            return selected.Sign;
        } else {
            return string.Empty;
        }
    }

    #endregion

    #region Utilities

    internal static void WriteLine()
    {
        Console.WriteLine();
    }

    internal static void WriteLine(string value)
    {
        PromptHelper.WriteMarkupLine(value);
    }

    internal static void WriteLine(int indent, string value)
    {
        var strIndent = new string(' ', indent * 2);
        PromptHelper.WriteMarkupLine(strIndent + value);
    }

    internal static void WriteLine(string value, params object[] args)
    {
        PromptHelper.WriteMarkupLine(string.Format(value, args));
    }

    internal static void WriteLine(int indent, string value, params object[] args)
    {
        var strIndent = new string(' ', indent * 2);
        PromptHelper.WriteMarkupLine(strIndent + string.Format(value, args));
    }

    internal static void LoadFile(BaseContext baseContext, string selectedFile)
    {
        WriteLine("Selected file: {0}", selectedFile);
        var result = baseContext.FileLoad(selectedFile, false).GetAwaiter().GetResult();
        WriteLine("Database loaded successfully. Records: {0}.", baseContext.Tree.RecordsCount);
    }

    internal static void SaveFile(BaseContext baseContext, string selectedFile)
    {
        WriteLine("Selected file: {0}", selectedFile);
        var result = baseContext.FileSave(selectedFile).GetAwaiter().GetResult();
        WriteLine("Database saved successfully.");
    }

    internal static GDMRecord SelectRecord(BaseContext baseContext, GDMRecordType recordType, string prompt, string yesMsg, string noMsg)
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
