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

[Table("graph_entities")]
public class GraphEntity
{
    [Column("entity_id"), PrimaryKey]
    public string EntityId { get; set; } // Normalized ID, for example: "loc:derevnya_kovalevo", "person:ivan_suslov"

    [Column("name")]
    public string Name { get; set; } // Readable name: "деревня Ковалево", "Суслов Иван Петрович"

    [Column("type")]
    public string Type { get; set; } // Type: PERSON, LOCATION, ARCHIVE, HISTORICAL_EVENT, SOCIAL_CLASS

    [Column("description")]
    public string Description { get; set; } = string.Empty; // Additional info (years of existence, coordinates, etc.)

    [Column("created_at")]
    public DateTime CreatedAt { get; set; } = DateTime.UtcNow;
}


[Table("graph_relations")]
public class GraphRelation
{
    [Column("relation_id"), PrimaryKey, AutoIncrement]
    public int RelationId { get; set; }

    [Column("source_entity_id"), Indexed]
    public string SourceEntityId { get; set; }

    [Column("predicate")]
    public string Predicate { get; set; } // Relation type: BORN_IN, BELONGED_TO_PARISH, STORED_IN, MARRIED_TO, MIGRATED_TO

    [Column("target_entity_id"), Indexed]
    public string TargetEntityId { get; set; }

    [Column("context_notes")]
    public string ContextNotes { get; set; } = string.Empty; // Clarification (for example, the years of validity of this connection: "from 1890 to 1915")
}
