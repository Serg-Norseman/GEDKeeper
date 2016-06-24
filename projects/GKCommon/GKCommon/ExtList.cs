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
    [Serializable]
    public class ListException : Exception
    {
        public ListException()
        {
        }
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

        public IList<T> List
        {
            get { return this.fList; }
        }

        public int Count
        {
            get { return this.fList.Count; }
        }

        public bool OwnsObjects
        {
            get { return this.fOwnsObjects; }
            set { this.fOwnsObjects = value; }
        }

        public T this[int index]
        {
            get {
                return this.fList[index];
            }
            set {
                if (index < 0 || index >= this.Count)
                {
                    Error("List index out of bounds ({0})", index);
                }

                if (Equals(value, this.fList[index])) return;

                T temp = this.fList[index];
                this.fList[index] = value;

                if (temp != null)
                {
                    this.Notify(temp, ListNotification.Deleted);
                }

                if (value != null)
                {
                    this.Notify(value, ListNotification.Added);
                }
            }
        }

        public ExtList()
        {
            this.fList = new List<T>();
            this.fOwnsObjects = false;
        }

        public ExtList(bool ownsObjects)
        {
            this.fList = new List<T>();
            this.fOwnsObjects = ownsObjects;
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing)
            {
                //if (this.fList != null) this.fList = null;
                this.Clear();
            }
            base.Dispose(disposing);
        }

        private void Notify(object instance, ListNotification action)
        {
            if (this.fOwnsObjects && action == ListNotification.Deleted)
            {
                if (instance is IDisposable)
                {
                    (instance as IDisposable).Dispose();
                }
            }
        }

        public int Add(T item)
        {
            int result = this.fList.Count;
            this.fList.Add(item);
            if (item != null)
            {
                this.Notify(item, ListNotification.Added);
            }
            return result;
        }

        public void Clear()
        {
            for (int i = this.fList.Count - 1; i >= 0; i--) this.Notify(fList[i], ListNotification.Deleted);
            this.fList.Clear();
        }

        public void Delete(int index)
        {
            object temp = this.fList[index];

            this.fList.RemoveAt(index);

            if (temp != null)
            {
                this.Notify(temp, ListNotification.Deleted);
            }
        }

        private static void Error(string msg, int data)
        {
            throw new ListException(string.Format(msg, data));
        }

        public void Exchange(int index1, int index2)
        {
            T item = this.fList[index1];
            this.fList[index1] = this.fList[index2];
            this.fList[index2] = item;
        }

        public object Extract(T item)
        {
            object result = null;
            int I = this.IndexOf(item);
            if (I >= 0)
            {
                result = item;
                this.fList.RemoveAt(I);
                this.Notify(result, ListNotification.Extracted);
            }
            return result;
        }

        public int IndexOf(T item)
        {
            return this.fList.IndexOf(item);
        }

        public void Insert(int index, T item)
        {
            this.fList.Insert(index, item);
            if (item != null)
            {
                this.Notify(item, ListNotification.Added);
            }
        }

        public int Remove(T item)
        {
            int result = this.IndexOf(item);
            if (result >= 0)
            {
                this.Delete(result);
            }
            return result;
        }

        public void Pack()
        {
            for (int I = this.Count - 1; I >= 0; I--)
            {
                if (this[I] == null)
                    this.Delete(I);
            }
        }

        public void QuickSort(Comparison<T> comparer)
        {
            SortHelper.QuickSort(fList, comparer);
        }

        public void MergeSort(Comparison<T> comparer)
        {
            SortHelper.MergeSort(fList, comparer);
        }
    }
}
