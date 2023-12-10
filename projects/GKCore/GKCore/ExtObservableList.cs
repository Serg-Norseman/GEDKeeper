/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2022 by Sergey V. Zhdanovskih.
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

using System.Collections;
using System.Collections.Generic;
using System.Collections.Specialized;
using System.ComponentModel;

namespace GKCore
{
    public interface IUpdatableCollection : ICollection
    {
        void BeginUpdate();
        void EndUpdate();
    }


    /// <summary>
    /// 
    /// </summary>
    public class ExtObservableList<T> : List<T>, IUpdatableCollection, INotifyCollectionChanged, INotifyPropertyChanged
    {
        private int fUpdateCount;

        public event NotifyCollectionChangedEventHandler CollectionChanged;

        event PropertyChangedEventHandler INotifyPropertyChanged.PropertyChanged
        {
            add {
                PropertyChanged += value;
            }
            remove {
                PropertyChanged -= value;
            }
        }

        public ExtObservableList()
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

        public void Replace(T item, int index)
        {
            ChangeReplaced(item, index);
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

        public void Reset()
        {
            ChangeReset();
        }

        #region Private methods

        private void OnCollectionChanged(NotifyCollectionChangedEventArgs args)
        {
            var handler = CollectionChanged;
            if (handler != null && fUpdateCount == 0)
                handler(this, args);
        }

        private void ChangeReset()
        {
            OnPropertyChanged(CountProp);
            OnPropertyChanged(IndexerProp);
            OnCollectionChanged(new NotifyCollectionChangedEventArgs(NotifyCollectionChangedAction.Reset));
        }

        private void ChangeAdded(T item, int index)
        {
            OnPropertyChanged(CountProp);
            OnPropertyChanged(IndexerProp);
            OnCollectionChanged(new NotifyCollectionChangedEventArgs(NotifyCollectionChangedAction.Add, item, index));
        }

        private void ChangeRemoved(T item, int index)
        {
            OnPropertyChanged(CountProp);
            OnPropertyChanged(IndexerProp);
            OnCollectionChanged(new NotifyCollectionChangedEventArgs(NotifyCollectionChangedAction.Remove, item, index));
        }

        private void ChangeMoved(T item, int oldIndex, int newIndex)
        {
            OnPropertyChanged(IndexerProp);
            OnCollectionChanged(new NotifyCollectionChangedEventArgs(NotifyCollectionChangedAction.Move, item, newIndex, oldIndex));
        }

        private void ChangeReplaced(T item, int index)
        {
            OnPropertyChanged(IndexerProp);
            OnCollectionChanged(new NotifyCollectionChangedEventArgs(NotifyCollectionChangedAction.Replace, item, item, index));
        }

        private void OnPropertyChanged(string propertyName)
        {
            var handler = PropertyChanged;
            if (handler != null)
                handler(this, new PropertyChangedEventArgs(propertyName));
        }

        protected virtual event PropertyChangedEventHandler PropertyChanged;

        private const string CountProp = "Count";
        private const string IndexerProp = "Item[]";

        #endregion
    }
}
