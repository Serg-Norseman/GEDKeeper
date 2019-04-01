/* CIndexPage.cs
 * 
 * Copyright 2009 Alexander Curtis <alex@logicmill.com>
 * This file is part of GEDmill - A family history website creator
 * 
 * GEDmill is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * GEDmill is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with GEDmill.  If not, see <http://www.gnu.org/licenses/>.
 *
 *
 * History:  
 * 10Dec08 AlexC          Migrated from GEDmill 1.10
 *
 */

using System.Collections.Generic;

namespace GEDmill.HTML
{
    /// <summary>
    /// Data structure holding all the entries in an index page. (Significant for indexes that spread over multiple pages.)
    /// </summary>
    public class IndexPage
    {
        public List<IndexLetter> Letters;
        public string FileName;
        public int TotalIndis;


        public IndexPage(string filename)
        {
            FileName = filename;
            Letters = new List<IndexLetter>();
            TotalIndis = 0;
        }
    }
}
