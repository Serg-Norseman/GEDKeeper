/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2017 by Sergey V. Zhdanovskih.
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
    public class ListException : Exception
    {
        public ListException(string message) : base(message)
        {
        }
    }

    public enum ListNotification
    {
        Added,
        Extracted,
        Deleted
    }

    /// <summary>
    /// 
    /// </summary>
    public class ExtList<T> : BaseObject where T : class
    {
        private readonly List<T> fList;
        private bool fOwnsObjects;

        /*public IList<T> List
        {
            get { return this.fList; }
        }*/

        public int Count
        {
            get { return fList.Count; }
        }

        public bool OwnsObjects
        {
            get { return fOwnsObjects; }
            set { fOwnsObjects = value; }
        }

        public T this[int index]
        {
            get {
                return fList[index];
            }
            set {
                if (index < 0 || index >= fList.Count)
                    throw new ListException(string.Format("List index out of bounds ({0})", index));

                if (Equals(value, fList[index])) return;

                T temp = fList[index];
                fList[index] = value;

                if (temp != null)
                {
                    Notify(temp, ListNotification.Deleted);
                }

                if (value != null)
                {
                    Notify(value, ListNotification.Added);
                }
            }
        }

        public ExtList()
        {
            fList = new List<T>();
            fOwnsObjects = false;
        }

        public ExtList(bool ownsObjects)
        {
            fList = new List<T>();
            fOwnsObjects = ownsObjects;
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing)
            {
                //if (this.fList != null) this.fList = null;
                Clear();
            }
            base.Dispose(disposing);
        }

        private void Notify(object instance, ListNotification action)
        {
            if (fOwnsObjects && action == ListNotification.Deleted)
            {
                var disposable = instance as IDisposable;
                if (disposable != null) {
                    disposable.Dispose();
                }
            }
        }

        public int Add(T item)
        {
            int result = fList.Count;
            fList.Add(item);
            if (item != null)
            {
                Notify(item, ListNotification.Added);
            }
            return result;
        }

        public void Clear()
        {
            for (int i = fList.Count - 1; i >= 0; i--) Notify(fList[i], ListNotification.Deleted);
            fList.Clear();
        }

        public void Delete(int index)
        {
            object temp = fList[index];

            fList.RemoveAt(index);

            if (temp != null)
            {
                Notify(temp, ListNotification.Deleted);
            }
        }

        public void Exchange(int index1, int index2)
        {
            T item = fList[index1];
            fList[index1] = fList[index2];
            fList[index2] = item;
        }

        public object Extract(T item)
        {
            object result = null;
            int I = IndexOf(item);
            if (I >= 0)
            {
                result = item;
                fList.RemoveAt(I);
                Notify(result, ListNotification.Extracted);
            }
            return result;
        }

        public int IndexOf(T item)
        {
            return fList.IndexOf(item);
        }

        public void Insert(int index, T item)
        {
            fList.Insert(index, item);
            if (item != null) {
                Notify(item, ListNotification.Added);
            }
        }

        public int Remove(T item)
        {
            int result = IndexOf(item);
            if (result >= 0)
            {
                Delete(result);
            }
            return result;
        }

        public void Pack()
        {
            for (int I = fList.Count - 1; I >= 0; I--)
            {
                if (this[I] == null)
                    Delete(I);
            }
        }

        public void QuickSort(Comparison<T> comparer)
        {
            SysUtils.QuickSort(fList, comparer);
        }

        public void MergeSort(Comparison<T> comparer)
        {
            SysUtils.MergeSort(fList, comparer);
        }
    }
}
