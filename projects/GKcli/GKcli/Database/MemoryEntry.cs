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

public sealed class MemoryEntry
{
    [PrimaryKey, AutoIncrement]
    public int Id { get; set; }
    public string Content { get; set; }
    public byte[] Embedding { get; set; }
    public DateTime CreatedAt { get; set; }
}
