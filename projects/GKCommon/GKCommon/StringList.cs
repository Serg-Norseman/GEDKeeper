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
using System.Text;

namespace GKCommon
{
    [Serializable]
    public class StringListException : Exception
    {
        public StringListException(string message) : base(message)
        {
        }
    }

    /*
     * I.e. EventHandler
     */
    public delegate void NotifyEventHandler(object sender /*, EventArgs e*/);

    public enum DuplicateSolve
    {
        Ignore,
        Accept,
        Error
    }

    /// <summary>
    /// 
    /// </summary>
    public sealed class StringList : BaseObject
    {
        private sealed class StringItem
        {
            public string StrVal;
            public object ObjVal;

            public StringItem(string str, object obj)
            {
                StrVal = str;
                ObjVal = obj;
            }
        }

        private readonly List<StringItem> fList;
        private bool fCaseSensitive;
        private DuplicateSolve fDuplicateSolve;
        private NotifyEventHandler fOnChange;
        private NotifyEventHandler fOnChanging;
        private bool fSorted;
        private int fUpdateCount;

        private const string LINE_BREAK = "\r\n";

        public bool CaseSensitive
        {
            get {
                return fCaseSensitive;
            }
            set {
                if (value == fCaseSensitive) return;
                fCaseSensitive = value;
                if (fSorted) Sort();
            }
        }

        public int Count
        {
            get { return fList.Count; }
        }

        public string this[int index]
        {
            get {
                if (index < 0 || index >= fList.Count)
                    throw new StringListException(string.Format("List index out of bounds ({0})", index));

                return fList[index].StrVal;
            }

            set {
                if (fSorted)
                    throw new StringListException("Operation not allowed on sorted list");

                if (index < 0 || index >= fList.Count)
                    throw new StringListException(string.Format("List index out of bounds ({0})", index));

                Changing();
                fList[index].StrVal = value;
                Changed();
            }
        }

        public DuplicateSolve DuplicateSolve
        {
            get { return fDuplicateSolve; }
            set { fDuplicateSolve = value; }
        }

        public event NotifyEventHandler OnChange
        {
            add { fOnChange = value; }
            remove { if (fOnChange == value) fOnChange = null; }
        }

        public event NotifyEventHandler OnChanging
        {
            add { fOnChanging = value; }
            remove { if (fOnChanging == value) fOnChanging = null; }
        }

        public bool Sorted
        {
            get {
                return fSorted;
            }
            set {
                if (fSorted == value) return;
                if (value) Sort();
                fSorted = value;
            }
        }

        public string Text
        {
            get { return GetTextStr(); }
            set { SetTextStr(value); }
        }


        public StringList()
        {
            fList = new List<StringItem>();
        }

        public StringList(string str) : this()
        {
            SetTextStr(str);
        }

        public StringList(string[] list) : this()
        {
            if (list == null)
                throw new ArgumentNullException("list");

            for (int i = 0; i < list.Length; i++)
            {
                AddObject(list[i], null);
            }
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing)
            {
                //this.fList = null;
                /*int num = this.fList.Count;
                for (int i = 0; i < num; i++)
                {
                    IDisposable inst = this.fList[i].FObject as IDisposable;
                    if (inst != null) inst.Dispose();
                }*/
            }
            base.Dispose(disposing);
        }

        public bool IsEmpty()
        {
            return (fList.Count <= 0);
        }

        public object GetObject(int index)
        {
            if (index < 0 || index >= fList.Count)
                throw new StringListException(string.Format("List index out of bounds ({0})", index));

            return fList[index].ObjVal;
        }

        public void SetObject(int index, object obj)
        {
            if (index < 0 || index >= fList.Count)
                throw new StringListException(string.Format("List index out of bounds ({0})", index));

            Changing();
            fList[index].ObjVal = obj;
            Changed();
        }

        private string GetTextStr()
        {
            StringBuilder buffer = new StringBuilder();

            int num = fList.Count;
            for (int i = 0; i < num; i++)
            {
                buffer.Append(this[i]);
                buffer.Append(LINE_BREAK);
            }

            return buffer.ToString();
        }

        private void SetTextStr(string value)
        {
            BeginUpdate();
            try
            {
                Clear();

                int start = 0;
                int lbLen = LINE_BREAK.Length;
                int pos = value.IndexOf(LINE_BREAK);
                
                while (pos >= 0)
                {
                    string s = value.Substring(start, pos - start);
                    Add(s);
                    start = pos + lbLen;
                    pos = value.IndexOf(LINE_BREAK, start);
                }

                if (start <= value.Length)
                {
                    string s = value.Substring(start, (value.Length - start));
                    Add(s);
                }
            }
            finally
            {
                EndUpdate();
            }
        }

        public int Add(string str)
        {
            return AddObject(str, null);
        }

        public int AddObject(string str, object obj)
        {
            int result;

            if (!fSorted) {
                result = fList.Count;
            } else {
                if (Find(str, out result)) {
                    if (fDuplicateSolve == DuplicateSolve.Ignore)
                        return result;

                    if (fDuplicateSolve == DuplicateSolve.Error)
                        throw new StringListException("String list does not allow duplicates");
                }
            }

            InsertItem(result, str, obj);

            return result;
        }

        public void AddStrings(StringList strList)
        {
            if (strList == null) return;

            BeginUpdate();
            try
            {
                int num = strList.Count;
                for (int i = 0; i < num; i++) {
                    AddObject(strList[i], strList.GetObject(i));
                }
            }
            finally
            {
                EndUpdate();
            }
        }

        public void Assign(StringList source)
        {
            if (source == null) return;

            BeginUpdate();
            try
            {
                Clear();
                AddStrings(source);
            }
            finally
            {
                EndUpdate();
            }
        }

        public void Clear()
        {
            if (fList.Count == 0) return;

            Changing();
            fList.Clear();
            Changed();
        }

        public void Delete(int index)
        {
            if (index < 0 || index >= fList.Count)
                throw new StringListException(string.Format("List index out of bounds ({0})", index));

            Changing();
            if (index < fList.Count)
            {
                fList.RemoveAt(index);
            }
            Changed();
        }

        public void Exchange(int index1, int index2)
        {
            if (index1 < 0 || index1 >= fList.Count)
                throw new StringListException(string.Format("List index out of bounds ({0})", index1));

            if (index2 < 0 || index2 >= fList.Count)
                throw new StringListException(string.Format("List index out of bounds ({0})", index2));

            Changing();
            ExchangeItems(index1, index2);
            Changed();
        }

        public void Insert(int index, string str)
        {
            InsertObject(index, str, null);
        }

        public void InsertObject(int index, string str, object obj)
        {
            if (fSorted)
                throw new StringListException("Operation not allowed on sorted list");

            if (index < 0 || index > fList.Count)
                throw new StringListException(string.Format("List index out of bounds ({0})", index));

            InsertItem(index, str, obj);
        }

        private void InsertItem(int index, string str, object obj)
        {
            Changing();
            fList.Insert(index, new StringItem(str, obj));
            Changed();
        }

        public void ExchangeItems(int index1, int index2)
        {
            StringItem temp = fList[index1];
            fList[index1] = fList[index2];
            fList[index2] = temp;
        }

        public string[] ToArray()
        {
            int len = fList.Count;
            string[] result = new string[len];
            for (int i = 0; i < len; i++) {
                result[i] = this[i];
            }
            return result;
        }

        #region Updating

        private void SetUpdateState(bool updating)
        {
            if (updating) {
                Changing();
            } else {
                Changed();
            }
        }

        public void BeginUpdate()
        {
            if (fUpdateCount == 0) {
                SetUpdateState(true);
            }
            fUpdateCount++;
        }

        public void EndUpdate()
        {
            fUpdateCount--;
            if (fUpdateCount == 0) {
                SetUpdateState(false);
            }
        }

        private void Changed()
        {
            if (fUpdateCount == 0 && fOnChange != null) {
                fOnChange(this);
            }
        }

        private void Changing()
        {
            if (fUpdateCount == 0 && fOnChanging != null) {
                fOnChanging(this);
            }
        }

        #endregion

        #region Search

        public int IndexOfObject(object obj)
        {
            int num = fList.Count;
            for (int i = 0; i < num; i++) {
                if (fList[i].ObjVal == obj) {
                    return i;
                }
            }

            return -1;
        }

        public int IndexOf(string str)
        {
            int result = -1;
            if (fList.Count <= 0) return result;

            if (!fSorted) {
                int num = fList.Count;
                for (int i = 0; i < num; i++) {
                    if (CompareStrings(fList[i].StrVal, str) == 0) {
                        result = i;
                        break;
                    }
                }
            } else {
                if (!Find(str, out result)) {
                    result = -1;
                }
            }

            return result;
        }

        /// <summary>
        /// Important: `index`, returned by this method, necessary in the
        /// methods of insertion for sorted lists - even when the search
        /// string is not found.
        /// </summary>
        /// <param name="str"></param>
        /// <param name="index"></param>
        /// <returns></returns>
        private bool Find(string str, out int index)
        {
            bool result = false;

            int low = 0;
            int high = fList.Count - 1;

            while (low <= high) {
                int idx = (int)((uint)(low + high) >> 1);
                int cmp = CompareStrings(fList[idx].StrVal, str);

                if (cmp < 0) {
                    low = idx + 1;
                } else {
                    high = idx - 1;

                    if (cmp == 0) {
                        result = true;

                        if (fDuplicateSolve != DuplicateSolve.Accept) {
                            low = idx;
                        }
                    }
                }
            }

            index = low;

            return result;
        }

        #endregion

        #region Sorting

        private int CompareStrings(string s1, string s2)
        {
            return string.Compare(s1, s2, !fCaseSensitive);
        }

        private int CompareItems(StringItem item1, StringItem item2)
        {
            return string.Compare(item1.StrVal, item2.StrVal, !fCaseSensitive);
        }

        public void Sort()
        {
            if (!fSorted && fList.Count > 1)
            {
                Changing();
                SysUtils.QuickSort(fList, CompareItems);
                Changed();
            }
        }

        #endregion
    }
}
