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
using System.Text.Json;
using GKcli.Commands;
using GKcli.MCP;
using GKcli.Resources;
using GKCore;
using GKUI.Platform;
using Sharprompt;

namespace GKcli;

internal class CommandController
{
    private static readonly BaseContext fBaseContext = new BaseContext(null);
    private static readonly Dictionary<string, BaseCommand> fCommands = new Dictionary<string, BaseCommand>();
    private static readonly Dictionary<string, BaseResource> fResources = new Dictionary<string, BaseResource>();
    private static Dictionary<string, object> fVariables = new Dictionary<string, object>();

    public const string CMD_RETURN = "return";
    public const string CMD_EXIT = "exit";

    static CommandController()
    {
    }

    public static void InitCommands(bool mcp, bool pure)
    {
        // Files operations
        RegisterCommand(new FileMenuCommand());
        RegisterCommand(new FileNewCommand());
        RegisterCommand(new FileLoadCommand());
        RegisterCommand(new FileLoadRecentCommand());
        RegisterCommand(new FileSaveCommand());
        RegisterCommand(new FileSaveAsCommand());
        RegisterCommand(new FilePropsCommand());
        RegisterCommand(new FileRecentCommand());
        RegisterCommand(new FileReloadCommand());
        RegisterCommand(new FileSearchCommand());
        RegisterCommand(new FileValidateCommand());

        // Records operations
        RegisterCommand(new RecordListCommand());
        RegisterCommand(new RecordInfoCommand());
        RegisterCommand(new RecordDeleteCommand());
        RegisterCommand(new RecordSearchCommand());
        RegisterCommand(new RecordSetRestrictionCommand());

        RegisterCommand(new RecordAddUserRefCommand());
        RegisterCommand(new RecordDeleteUserRefCommand());
        RegisterCommand(new RecordListUserRefsCommand());

        RegisterCommand(new RecordAddSourceCitationCommand());
        RegisterCommand(new RecordDeleteSourceCitationCommand());
        RegisterCommand(new RecordListSourceCitationsCommand());

        RegisterCommand(new RecordAddMultimediaLinkCommand());
        RegisterCommand(new RecordDeleteMultimediaLinkCommand());
        RegisterCommand(new RecordListMultimediaCommand());

        RegisterCommand(new RecordAddNoteCommand());
        RegisterCommand(new RecordDeleteNoteCommand());
        RegisterCommand(new RecordListNotesCommand());

        // Events
        RegisterCommand(new EventEditCommand());
        RegisterCommand(new EventTypeListCommand());

        // Individuals operations
        RegisterCommand(new IndiMenuCommand());
        RegisterCommand(new IndiListCommand());
        RegisterCommand(new IndiSearchCommand());
        RegisterCommand(new IndiAddCommand());
        RegisterCommand(new IndiEditCommand());
        RegisterCommand(new IndividualUpsertCommand());
        RegisterCommand(new IndiDeleteCommand());

        RegisterCommand(new IndiListSpousesCommand()); // control through family tools
        RegisterCommand(new IndiListGroupsCommand()); // control through group tools

        RegisterCommand(new IndiListAssociationsCommand());
        RegisterCommand(new IndiAddAssociationCommand());
        RegisterCommand(new IndiEditAssociationCommand());
        RegisterCommand(new IndiDeleteAssociationCommand());

        RegisterCommand(new IndiListEventsCommand());
        RegisterCommand(new IndiAddEventCommand());
        RegisterCommand(new IndiEditEventCommand());
        RegisterCommand(new IndiDeleteEventCommand());

        RegisterCommand(new IndiListPersonalNamesCommand());
        RegisterCommand(new IndiAddPersonalNameCommand());
        RegisterCommand(new IndiEditPersonalNameCommand());
        RegisterCommand(new IndiDeletePersonalNameCommand());

        // Families operations
        RegisterCommand(new FamMenuCommand());
        RegisterCommand(new FamListCommand());
        RegisterCommand(new FamAddCommand());
        RegisterCommand(new FamEditCommand());
        RegisterCommand(new FamilyUpsertCommand());
        RegisterCommand(new FamDeleteCommand());

        RegisterCommand(new FamAddChildCommand());
        RegisterCommand(new FamDeleteChildCommand());
        RegisterCommand(new FamListChildrenCommand());

        RegisterCommand(new FamListEventsCommand());
        RegisterCommand(new FamAddEventCommand());
        RegisterCommand(new FamEditEventCommand());
        RegisterCommand(new FamDeleteEventCommand());

        // Notes operations
        RegisterCommand(new NoteMenuCommand());
        RegisterCommand(new NoteListCommand());
        RegisterCommand(new NoteAddCommand());
        RegisterCommand(new NoteEditCommand());
        RegisterCommand(new NoteUpsertCommand());
        RegisterCommand(new NoteDeleteCommand());

        // Multimedia operations
        RegisterCommand(new MediaMenuCommand());
        RegisterCommand(new MediaListCommand());
        RegisterCommand(new MediaAddCommand());
        RegisterCommand(new MediaEditCommand());
        RegisterCommand(new MediaDeleteCommand());
        RegisterCommand(new MediaGetCommand());

        RegisterCommand(new MediaListFilesCommand());
        RegisterCommand(new MediaAddFileCommand());
        RegisterCommand(new MediaEditFileCommand());
        RegisterCommand(new MediaDeleteFileCommand());

        // Sources operations
        RegisterCommand(new SourceMenuCommand());
        RegisterCommand(new SourceListCommand());
        RegisterCommand(new SourceAddCommand());
        RegisterCommand(new SourceEditCommand());
        RegisterCommand(new SourceUpsertCommand());
        RegisterCommand(new SourceDeleteCommand());

        RegisterCommand(new SourceListRepositoriesCommand());
        RegisterCommand(new SourceAddRepositoryCommand());
        RegisterCommand(new SourceDeleteRepositoryCommand());

        // Repositories operations
        RegisterCommand(new RepositoryMenuCommand());
        RegisterCommand(new RepositoryListCommand());
        RegisterCommand(new RepositoryAddCommand());
        RegisterCommand(new RepositoryEditCommand());
        RegisterCommand(new RepositoryDeleteCommand());

        // All 100+ instruments, including the system prompt, consume over 16,000 tokens.
        // Until optimization or a transition to a different approach is implemented, a limiter is needed.
        if (!mcp || !pure) {
            // Groups operations
            RegisterCommand(new GroupListCommand());
            RegisterCommand(new GroupAddCommand());
            RegisterCommand(new GroupEditCommand());
            RegisterCommand(new GroupDeleteCommand());

            RegisterCommand(new GroupListMembersCommand());
            RegisterCommand(new GroupAddMemberCommand());
            RegisterCommand(new GroupDeleteMemberCommand());

            // Tasks operations
            RegisterCommand(new TaskListCommand());
            RegisterCommand(new TaskAddCommand());
            RegisterCommand(new TaskEditCommand());
            RegisterCommand(new TaskUpsertCommand());
            RegisterCommand(new TaskDeleteCommand());

            // Researches operations
            RegisterCommand(new ResearchListCommand());
            RegisterCommand(new ResearchAddCommand());
            RegisterCommand(new ResearchEditCommand());
            RegisterCommand(new ResearchDeleteCommand());

            RegisterCommand(new ResearchListTasksCommand());
            RegisterCommand(new ResearchAddTaskCommand());
            RegisterCommand(new ResearchDeleteTaskCommand());

            RegisterCommand(new ResearchListCommunicationsCommand());
            RegisterCommand(new ResearchAddCommunicationCommand());
            RegisterCommand(new ResearchDeleteCommunicationCommand());

            RegisterCommand(new ResearchListGroupsCommand());
            RegisterCommand(new ResearchAddGroupCommand());
            RegisterCommand(new ResearchDeleteGroupCommand());

            // Communications operations
            RegisterCommand(new CommunicationListCommand());
            RegisterCommand(new CommunicationAddCommand());
            RegisterCommand(new CommunicationEditCommand());
            RegisterCommand(new CommunicationDeleteCommand());

            // Locations operations
            RegisterCommand(new LocationListCommand());
            RegisterCommand(new LocationAddCommand());
            RegisterCommand(new LocationEditCommand());
            RegisterCommand(new LocationDeleteCommand());

            RegisterCommand(new LocationListNamesCommand());
            RegisterCommand(new LocationAddNameCommand());
            RegisterCommand(new LocationEditNameCommand());
            RegisterCommand(new LocationDeleteNameCommand());

            RegisterCommand(new LocationListTopLinksCommand());
            RegisterCommand(new LocationAddTopLinkCommand());
            RegisterCommand(new LocationEditTopLinkCommand());
            RegisterCommand(new LocationDeleteTopLinkCommand());
        }

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

        // Resources
        RegisterCommand(new GEDCOMDateSpecCommand());
        RegisterResource(new GEDCOMDateSpecResource());
    }

    private static void RegisterCommand(BaseCommand commandInstance)
    {
        fCommands.Add(commandInstance.Sign, commandInstance);
    }

    private static void RegisterResource(BaseResource resource)
    {
        fResources.Add(resource.Uri, resource);
    }

    public static string SelectCommand(CommandCategory category, bool hasReturn, string message)
    {
        // For commands implemented in MCP but not implemented for CLI, there will be no name.
        var cmdList = fCommands.Values.Where(c => (c.Category == category && c.Text != "#")).ToList();
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

    internal static IEnumerable<BaseCommand> GetCommands()
    {
        return fCommands.Values;
    }

    public static List<MCPContent> ExecuteTool(string toolName, JsonElement args)
    {
        if (fCommands.TryGetValue(toolName, out BaseCommand cmd)) {
            return cmd.ExecuteTool(fBaseContext, args);
        } else {
            throw new ArgumentException($"Unknown tool: {toolName}");
        }
    }

    internal static IEnumerable<BaseResource> GetResources()
    {
        return fResources.Values;
    }

    public static List<MCPResourceContents> GetResource(string uri)
    {
        if (fResources.TryGetValue(uri, out BaseResource res)) {
            return res.Get(fBaseContext);
        } else {
            return null;
        }
    }
}
