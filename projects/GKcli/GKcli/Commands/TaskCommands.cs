/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using GDModel;
using GKCore;
using GKCore.Locales;
using GKUI.Platform;

namespace GKcli.Commands;

internal class TaskMenuCommand : BaseCommand
{
    public TaskMenuCommand() : base("tasks", LSID.RPTasks, CommandCategory.Application) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        CommandController.SelectCommand(CommandCategory.Task, true, "Select a task operation");
    }
}


internal class TaskListCommand : BaseCommand
{
    public TaskListCommand() : base("task_list", LSID.Find, CommandCategory.Task) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        PromptHelper.SelectRecord(baseContext, GDMRecordType.rtTask, "Select a task", "Task: {0}", "No records.");
    }
}


internal class TaskAddCommand : BaseCommand
{
    public TaskAddCommand() : base("task_add", null, CommandCategory.Task) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Not implemented yet
    }
}


internal class TaskEditCommand : BaseCommand
{
    public TaskEditCommand() : base("task_edit", null, CommandCategory.Task) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Not implemented yet
    }
}


internal class TaskDeleteCommand : BaseCommand
{
    public TaskDeleteCommand() : base("task_delete", null, CommandCategory.Task) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Not implemented yet
    }
}
