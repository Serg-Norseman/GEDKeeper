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
using System.Collections;
using System.Collections.Generic;
using System.IO;
using Externals;

namespace GKCommon.GEDCOM
{
    public sealed class GEDCOMList<T> : IDisposable, IEnumerable where T : GEDCOMObject
    {
        #region ListEnumerator

        private struct GEDCOMListEnumerator : IGEDCOMListEnumerator
        {
            private readonly GEDCOMList<T> fOwnList;
            private int fIndex;
            private int fSize;

            public GEDCOMListEnumerator(GEDCOMList<T> list)
            {
                this.fOwnList = list;

                this.fIndex = -1;

                List<T> dataList = list.fDataList;
                this.fSize = ((dataList == null) ? 0 : dataList.Count);
            }

            void IEnumerator.Reset()
            {
                this.fIndex = -1;

                List<T> dataList = this.fOwnList.fDataList;
                this.fSize = ((dataList == null) ? 0 : dataList.Count);
            }

            bool IEnumerator.MoveNext()
            {
                this.fIndex++;
                return (this.fIndex < this.fSize);
            }

            object IEnumerator.Current
            {
                get { return this.fOwnList.fDataList[this.fIndex]; }
            }

            GEDCOMObject IGEDCOMListEnumerator.Owner
            {
                get { return this.fOwnList.fOwner; }
            }
        }

        #endregion

        
        private List<T> fDataList; // lazy implementation
        private readonly GEDCOMObject fOwner;
        private bool fDisposed;

        public int Count
        {
            get {
                return ((this.fDataList == null) ? 0 : this.fDataList.Count);
            }
        }

        public T this[int index]
        {
            get {
                return ((this.fDataList == null) ? default(T) : this.fDataList[index]);
            }
        }

        public GEDCOMObject Owner
        {
            get {
                return this.fOwner;
            }
        }
        
        public GEDCOMList(GEDCOMObject owner)
        {
            this.fOwner = owner;
            this.fDataList = null;
        }

        public void Dispose()
        {
            if (!this.fDisposed)
            {
                this.Clear();
                //this.fList.Free(); isnot IDisposable
                this.fDisposed = true;
            }
        }

        public IGEDCOMListEnumerator GetEnumerator()
        {
            return new GEDCOMListEnumerator(this);
        }

        IEnumerator IEnumerable.GetEnumerator()
        {
            return new GEDCOMListEnumerator(this);
        }
        
        public T Add(T item)
        {
            if (item != null)
            {
                if (this.fDataList == null)
                {
                    this.fDataList = new List<T>();
                }

                this.fDataList.Add(item);
            }

            return item;
        }

        public void Clear()
        {
            if (this.fDataList == null) return;

            for (int i = this.fDataList.Count - 1; i >= 0; i--)
            {
                this.fDataList[i].Dispose();
            }
            this.fDataList.Clear();
        }

        public int IndexOf(T item)
        {
            return (this.fDataList == null) ? -1 : this.fDataList.IndexOf(item);
        }

        public void DeleteAt(int index)
        {
            if (this.fDataList == null) return;
            
            this.fDataList[index].Dispose();
            this.fDataList.RemoveAt(index);
        }

        public void Delete(T item)
        {
            if (this.fDataList == null) return;

            int index = this.fDataList.IndexOf(item);
            if (index >= 0) {
                this.fDataList[index].Dispose();
                this.fDataList.RemoveAt(index);
            }
        }

        public void Exchange(int index1, int index2)
        {
            if (this.fDataList == null) return;

            if (index1 >= 0 && index1 < this.fDataList.Count && index2 >= 0 && index2 < this.fDataList.Count)
            {
                T tmp = this.fDataList[index1];
                this.fDataList[index1] = this.fDataList[index2];
                this.fDataList[index2] = tmp;
            }
        }

        public T Extract(int index)
        {
            if (this.fDataList == null) return default(T);

            T result = this.fDataList[index];
            this.fDataList.RemoveAt(index);
            return result;
        }

        public T Extract(T item)
        {
            if (this.fDataList == null) return default(T);

            int index = this.fDataList.IndexOf(item);
            if (index >= 0) {
                this.fDataList.RemoveAt(index);
                return item;
            } else {
                return default(T);
            }
        }

        public void SaveToStream(StreamWriter stream)
        {
            if (this.fDataList == null) return;

            int num = this.fDataList.Count;
            for (int i = 0; i < num; i++) {
                T item = this.fDataList[i];
                if (item is GEDCOMTag) {
                    (item as GEDCOMTag).SaveToStream(stream);
                }
            }
        }

        public void ReplaceXRefs(XRefReplacer map)
        {
            if (this.fDataList == null) return;

            int num = this.fDataList.Count;
            for (int i = 0; i < num; i++) {
                T item = this.fDataList[i];
                if (item is GEDCOMTag) {
                    (item as GEDCOMTag).ReplaceXRefs(map);
                }
            }
        }

        public void ResetOwner(GEDCOMTree newOwner)
        {
            if (this.fDataList == null) return;

            int num = this.fDataList.Count;
            for (int i = 0; i < num; i++)
            {
                GEDCOMTag item = this.fDataList[i] as GEDCOMTag;
                if (item != null)
                {
                    item.ResetOwner(newOwner);
                }
            }

            //this._owner = newOwner;
        }

        public void Pack()
        {
            if (this.fDataList == null) return;

            for (int i = this.fDataList.Count - 1; i >= 0; i--) {
                T item = this.fDataList[i];
                if (item is GEDCOMTag) {
                    GEDCOMTag tag = item as GEDCOMTag;
                    tag.Pack();
                    if (tag.IsEmpty() && tag.IsEmptySkip()) {
                        this.DeleteAt(i);
                    }
                }
            }
        }

        public void Sort(Comparison<T> comparer)
        {
            if (this.fDataList == null) return;

            ListTimSort<T>.Sort(this.fDataList, comparer);
        }
    }
}
