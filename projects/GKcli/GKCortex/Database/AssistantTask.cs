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

[Table("assistant_tasks")]
public class AssistantTask
{
    [Column("task_id"), PrimaryKey, AutoIncrement]
    public int TaskId { get; set; }

    [Column("status")]
    public string Status { get; set; } = "ACTIVE"; // ACTIVE, COMPLETED, PAUSED

    [Column("target_person")]
    public string TargetPerson { get; set; } = string.Empty;

    [Column("goal_description")]
    public string GoalDescription { get; set; }

    // Store as JSON strings, since SQLite does not support arrays directly.
    [Column("checked_sources_json")]
    public string CheckedSourcesJson { get; set; } = "[]";

    [Column("next_steps_json")]
    public string NextStepsJson { get; set; } = "[]";

    [Column("created_at")]
    public DateTime CreatedAt { get; set; } = DateTime.UtcNow;
}
