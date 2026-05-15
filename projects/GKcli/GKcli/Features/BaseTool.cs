/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System.Collections.Generic;
using System.Text.Json;
using GKcli.MCP;
using GKCore;

namespace GKcli.Features;

/// <summary>
/// Abstract base class for MCP-tools.
/// </summary>
internal abstract class BaseTool
{
    private readonly string fSign;

    /// <summary>
    /// Gets the internal identifier of the command.
    /// </summary>
    public string Sign
    {
        get { return fSign; }
    }

    public BaseTool()
    {
    }

    /// <summary>
    /// Constructor for the command.
    /// </summary>
    /// <param name="sign">Internal identifier of the command.</param>
    protected BaseTool(string sign)
    {
        fSign = sign;
    }


    /// <summary>
    /// Creates an MCP tool object to support the built-in MCP server.
    /// </summary>
    public virtual MCPTool CreateTool()
    {
        return null;
    }

    /// <summary>
    /// Runs MCP tool to support built-in MCP server.
    /// </summary>
    public virtual List<MCPContent> ExecuteTool(BaseContext baseContext, JsonElement args)
    {
        return null;
    }
}
