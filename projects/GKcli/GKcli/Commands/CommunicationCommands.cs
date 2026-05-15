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

internal class CommunicationMenuCommand : BaseCommand
{
    public CommunicationMenuCommand() : base("communications", LSID.RPCommunications, CommandCategory.Application) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        CommandController.SelectCommand(CommandCategory.Communication, true, "Select a communication operation");
    }
}


internal class CommunicationListCommand : BaseCommand
{
    public CommunicationListCommand() : base("communication_list", LSID.Find, CommandCategory.Communication) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        PromptHelper.SelectRecord(baseContext, GDMRecordType.rtCommunication, "Select a communication", "Communication: {0}", "No records.");
    }
}


internal class CommunicationAddCommand : BaseCommand
{
    public CommunicationAddCommand() : base("communication_add", null, CommandCategory.Communication) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Not implemented yet
    }
}


internal class CommunicationEditCommand : BaseCommand
{
    public CommunicationEditCommand() : base("communication_edit", null, CommandCategory.Communication) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Not implemented yet
    }
}


internal class CommunicationDeleteCommand : BaseCommand
{
    public CommunicationDeleteCommand() : base("communication_delete", null, CommandCategory.Communication) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Not implemented yet
    }
}
