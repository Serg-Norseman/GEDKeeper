/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using SQLite;

namespace GKcli.Database;

[Table("user_preferences")]
public class UserPreference
{
    [Column("pref_key"), PrimaryKey]
    public string PrefKey { get; set; } // For example: "research_focus"

    [Column("pref_value")]
    public string PrefValue { get; set; }

    [Column("confidence_score")]
    public double ConfidenceScore { get; set; } = 1.0;

    [Column("last_updated")]
    public DateTime LastUpdated { get; set; } = DateTime.UtcNow;
}
