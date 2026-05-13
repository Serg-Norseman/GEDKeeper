/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using SQLite;

namespace GKcli.Database;

internal sealed class ExtractionPattern
{
    [PrimaryKey, AutoIncrement]
    public int Id { get; set; }

    // Original text from the census (archaic)
    public string RawText { get; set; }

    // Ideal parsing result (JSON or structured text)
    public string CorrectedResult { get; set; }

    // Vector of this text (embedding)
    // Will be stored in the database as a string "0.12;0.45;..." or BLOB
    //public string Embedding { get; set; }
    public byte[] Embedding { get; set; }

    // Additional filters (century, region) to narrow your search
    public string Century { get; set; }
}
