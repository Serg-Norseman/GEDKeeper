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

using System.Collections.Generic;

namespace GEDmill.Model
{
    /// <summary>
    /// Data structure holding all the entries in the index under the given letter.
    /// Title is usually the same as initial, except for no-surname case.
    /// </summary>
    public class IndexLetter
    {
        public List<StringTuple> Items;
        public string Initial;
        public string Title;


        public IndexLetter(string initial, string title, List<StringTuple> letterList)
        {
            Initial = initial;
            Title = title;
            Items = letterList;
        }
    }
}
