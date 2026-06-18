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

[Table("memory_entries")]
public sealed class MemoryEntry
{
    [Column("id"), PrimaryKey, AutoIncrement]
    public int Id { get; set; }

    [Column("content")]
    public string Content { get; set; }

    [Column("embedding")]
    public byte[] Embedding { get; set; }

    [Column("created_at")]
    public DateTime CreatedAt { get; set; }
}
