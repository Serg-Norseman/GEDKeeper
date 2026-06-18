/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using SQLite;

namespace GKCortex.Database;

[Table("assistant_summary")]
public class AssistantSummary
{
    [Column("session_id"), PrimaryKey]
    public string SessionId { get; set; }

    [Column("global_summary")]
    public string GlobalSummary { get; set; } = string.Empty;

    [Column("current_session_summary")]
    public string CurrentSessionSummary { get; set; } = string.Empty;

    [Column("last_updated")]
    public DateTime LastUpdated { get; set; } = DateTime.UtcNow;
}
