/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System.Collections.Generic;
using GKcli.MCP;
using GKCore;

namespace GKcli.Resources;


/// <summary>
/// Abstract base class for resources.
/// </summary>
internal abstract class BaseResource
{
    protected readonly string fUri;

    public string Uri { get { return fUri; } }

    public BaseResource()
    {
    }

    protected BaseResource(string uri)
    {
        fUri = uri;
    }

    public virtual MCPResource CreateResource()
    {
        return null;
    }

    public virtual List<MCPResourceContents> Get(BaseContext baseContext)
    {
        return null;
    }
}
