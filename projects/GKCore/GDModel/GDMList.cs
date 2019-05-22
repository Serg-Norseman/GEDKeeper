/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2019 by Sergey V. Zhdanovskih.
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
using System.Collections;
using System.Collections.Generic;
using System.IO;

using BSLib;

namespace GKCommon.GEDCOM
{
    public sealed class GDMList<T> : IDisposable, IEnumerable<T> where T : GDMTag
    {
        #region ListEnumerator

        private struct GEDCOMListEnumerator : IGEDCOMListEnumerator<T>
        {
            private readonly GDMList<T> fOwnList;
            private int fIndex;
            private int fSize;

            public GEDCOMListEnumerator(GDMList<T> list)
            {
                fOwnList = list;

                fIndex = -1;

                List<T> dataList = list.fDataList;
                fSize = ((dataList == null) ? 0 : dataList.Count);
            }

            void IDisposable.Dispose()
            {
                // Stub. The method is required by the interface.
            }

            void IEnumerator.Reset()
            {
                fIndex = -1;

                List<T> dataList = fOwnList.fDataList;
                fSize = ((dataList == null) ? 0 : dataList.Count);
            }

            bool IEnumerator.MoveNext()
            {
                fIndex++;
                return (fIndex < fSize);
            }

            object IEnumerator.Current
            {
                get { return fOwnList.fDataList[fIndex]; }
            }

            T IEnumerator<T>.Current
            {
                get { return fOwnList.fDataList[fIndex]; }
            }
        }

        #endregion


        private List<T> fDataList; // lazy implementation


        public int Count
        {
            get {
                return ((fDataList == null) ? 0 : fDataList.Count);
            }
        }

        public T this[int index]
        {
            get {
                return ((fDataList == null) ? default(T) : fDataList[index]);
            }
        }
        

        public GDMList(GDMObject owner)
        {
            fDataList = null;
        }

        public void Dispose()
        {
            if (fDataList != null) {
                Clear();
                fDataList = null;
            }
        }

        internal IList<T> GetList()
        {
            return fDataList;
        }

        public IGEDCOMListEnumerator<T> GetEnumerator()
        {
            return new GEDCOMListEnumerator(this);
        }

        IEnumerator IEnumerable.GetEnumerator()
        {
            return new GEDCOMListEnumerator(this);
        }

        IEnumerator<T> IEnumerable<T>.GetEnumerator()
        {
            return new GEDCOMListEnumerator(this);
        }
        
        public T Add(T item)
        {
            if (item != null) {
                if (fDataList == null) {
                    fDataList = new List<T>();
                }

                fDataList.Add(item);
            }

            return item;
        }

        public void AddRange(GDMList<T> list)
        {
            if (list != null && list.fDataList != null) {
                if (fDataList == null) {
                    fDataList = new List<T>();
                }

                fDataList.AddRange(list.fDataList);
            }
        }

        public void Clear()
        {
            if (fDataList == null) return;

            for (int i = fDataList.Count - 1; i >= 0; i--) {
                fDataList[i].Dispose();
            }
            fDataList.Clear();
        }

        public int IndexOf(T item)
        {
            return (fDataList == null) ? -1 : fDataList.IndexOf(item);
        }

        public void DeleteAt(int index)
        {
            if (fDataList == null) return;
            
            fDataList[index].Dispose();
            fDataList.RemoveAt(index);
        }

        public void Delete(T item)
        {
            if (fDataList == null) return;

            int index = fDataList.IndexOf(item);
            if (index >= 0) {
                fDataList[index].Dispose();
                fDataList.RemoveAt(index);
            }
        }

        public void Exchange(int index1, int index2)
        {
            if (fDataList == null) return;

            if (index1 >= 0 && index1 < fDataList.Count && index2 >= 0 && index2 < fDataList.Count) {
                T tmp = fDataList[index1];
                fDataList[index1] = fDataList[index2];
                fDataList[index2] = tmp;
            }
        }

        public T Extract(int index)
        {
            if (fDataList == null) return default(T);

            T result = fDataList[index];
            fDataList.RemoveAt(index);
            return result;
        }

        public T Extract(T item)
        {
            if (fDataList == null) return default(T);

            int index = fDataList.IndexOf(item);
            if (index >= 0) {
                fDataList.RemoveAt(index);
                return item;
            } else {
                return default(T);
            }
        }

        public void SaveToStream(StreamWriter stream, int level)
        {
            if (fDataList == null) return;

            int num = fDataList.Count;
            for (int i = 0; i < num; i++) {
                var item = fDataList[i];
                if (item != null) {
                    item.SaveToStream(stream, level);
                }
            }
        }

        public void ReplaceXRefs(XRefReplacer map)
        {
            if (fDataList == null) return;

            int num = fDataList.Count;
            for (int i = 0; i < num; i++) {
                var item = fDataList[i];
                if (item != null) {
                    item.ReplaceXRefs(map);
                }
            }
        }

        public void Pack()
        {
            if (fDataList == null) return;

            for (int i = fDataList.Count - 1; i >= 0; i--) {
                var item = fDataList[i];
                if (item != null) {
                    item.Pack();
                    if (item.IsEmpty() && GEDCOMProvider.SkipEmptyTag(item.Name)) {
                        DeleteAt(i);
                    }
                }
            }
        }

        public void Sort(Comparison<T> comparer)
        {
            if (fDataList == null) return;

            ListTimSort<T>.Sort(fDataList, comparer);
        }
    }
}
