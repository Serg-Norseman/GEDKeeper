﻿/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2017 by Sergey V. Zhdanovskih.
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

using System.Collections.Generic;
using System.Collections.Specialized;
using BSLib.Design.MVP.Controls;

using BSDListItem = BSLib.Design.MVP.Controls.IListItem;

namespace GKUI.Components
{
    /// <summary>
    /// 
    /// </summary>
    public class ObservableExtList<T> : List<T>, INotifyCollectionChanged, IListViewItems where T : BSDListItem
    {
        private int fUpdateCount;

        public event NotifyCollectionChangedEventHandler CollectionChanged;

        public IListItem this[int index]
        {
            get { return (BSDListItem)base[index]; }
        }

        public ObservableExtList()
        {
        }

        public new void Clear()
        {
            base.Clear();
            ChangeReset();
        }

        public new void Add(T item)
        {
            base.Add(item);
            ChangeAdded(item, this.Count - 1);
        }

        public new void Insert(int index, T item)
        {
            base.Insert(index, item);
            ChangeAdded(item, index);
        }

        public new bool Remove(T item)
        {
            int index = IndexOf(item);
            if (index >= 0) {
                RemoveAt(index);
                ChangeRemoved(item, index);
                return true;
            }
            return false;
        }

        public new void RemoveAt(int index)
        {
            T item = base[index];
            base.RemoveAt(index);
            ChangeRemoved(item, index);
        }

        public void BeginUpdate()
        {
            if (fUpdateCount == 0) {
            }

            fUpdateCount++;
        }

        public void EndUpdate()
        {
            fUpdateCount--;

            if (fUpdateCount == 0) {
                ChangeReset();
            }
        }

        #region Private methods

        private void ChangeReset()
        {
            if (CollectionChanged != null && fUpdateCount == 0)
                CollectionChanged(this, new NotifyCollectionChangedEventArgs(NotifyCollectionChangedAction.Reset));
        }

        private void ChangeAdded(T item, int index)
        {
            if (CollectionChanged != null && fUpdateCount == 0)
                CollectionChanged(this, new NotifyCollectionChangedEventArgs(NotifyCollectionChangedAction.Add, item, index));
        }

        private void ChangeRemoved(T item, int index)
        {
            if (CollectionChanged != null && fUpdateCount == 0)
                CollectionChanged(this, new NotifyCollectionChangedEventArgs(NotifyCollectionChangedAction.Remove, item, index));
        }

        private void ChangeMoved(T item, int oldIndex, int newIndex)
        {
            if (CollectionChanged != null && fUpdateCount == 0)
                CollectionChanged(this, new NotifyCollectionChangedEventArgs(NotifyCollectionChangedAction.Move, item, newIndex, oldIndex));
        }

        #endregion
    }
}
