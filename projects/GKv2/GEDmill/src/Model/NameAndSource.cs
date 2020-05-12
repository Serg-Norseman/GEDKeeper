/* 
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
 */

using GDModel;

namespace GEDmill.Model
{
    /// <summary>
    /// A data structure to encapsulate an individual's name and source records that confirm it.
    /// Also contains the HTML for the source reference superscript numbers for this particular name.
    /// Used as an element in the list of all an individual's names.
    /// </summary>
    public class NameAndSource
    {
        public string Name;
        public string SourceHtml;
        public GDMList<GDMSourceCitation> Sources;


        public NameAndSource(string name)
        {
            Name = name;
            SourceHtml = "";
            Sources = new GDMList<GDMSourceCitation>(null);
        }
    }
}
