/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2016 by Serg V. Zhdanovskih (aka Alchemist, aka Norseman).
 *
 *  This file is part of "GEDKeeper".
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

using System;
using System.Collections.Generic;

namespace GKCommon
{
    /// <summary>
    /// 
    /// </summary>
    public static class LinqHelper
    {
        public static IEnumerable<TResult> OfTypeIterator<TResult>(IEnumerable<TResult> source)
        {
            foreach (object current in source)
            {
                if (current is TResult)
                {
                    yield return (TResult)((object)current);
                }
            }
            yield break;
        }

        public static TSource SingleOrDefault<TSource>(IEnumerable<TSource> source)
        {
            if (source == null)
                throw new ArgumentNullException("source");

            IList<TSource> list = source as IList<TSource>;

            if (list != null)
            {
                switch (list.Count)
                {
                    case 0:
                        return default(TSource);
                    case 1:
                        return list[0];
                }
            }
            else
            {
                using (IEnumerator<TSource> enumerator = source.GetEnumerator())
                {
                    if (!enumerator.MoveNext())
                    {
                        TSource result = default(TSource);
                        return result;
                    }
                    TSource current = enumerator.Current;
                    if (!enumerator.MoveNext())
                    {
                        TSource result = current;
                        return result;
                    }
                }
            }

            throw new Exception("MoreThanOneElement");
        }

        public static T FirstOrDefault<T>(IList<T> list)
        {
            if (list == null) {
                throw new ArgumentNullException("list");
            }

            return (list.Count > 0) ? list[0] : default(T);
        }

        public static T LastOrDefault<T>(IList<T> list)
        {
            if (list == null) {
                throw new ArgumentNullException("list");
            }

            int count = list.Count;
            return (count > 0) ? list[count - 1] : default(T);
        }
    }
}
