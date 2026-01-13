/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System.Collections.Generic;
using System.Collections.Specialized;
using System.ComponentModel;

namespace GKCore.Utilities
{
    /// <summary>
    /// 
    /// </summary>
    public class ExtObservableList<T> : List<T>, INotifyCollectionChanged, INotifyPropertyChanged
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

            if (fUpdateCount == 0) Reset();
        }

        public new void Add(T item)
        {
            base.Add(item);

            if (fUpdateCount == 0) ChangeAdded(item, this.Count - 1);
        }

        public new void Insert(int index, T item)
        {
            base.Insert(index, item);

            if (fUpdateCount == 0) ChangeAdded(item, index);
        }

        public new bool Remove(T item)
        {
            int index = IndexOf(item);
            if (index >= 0) {
                RemoveAt(index);

                if (fUpdateCount == 0) ChangeRemoved(item, index);

                return true;
            }
            return false;
        }

        public new void RemoveAt(int index)
        {
            T item = base[index];
            base.RemoveAt(index);

            if (fUpdateCount == 0) ChangeRemoved(item, index);
        }

        public void Replace(T item, int index)
        {
            if (fUpdateCount == 0) ChangeReplaced(item, index);
        }

        public void BeginUpdate()
        {
            fUpdateCount++;
        }

        public void EndUpdate()
        {
            fUpdateCount--;
            if (fUpdateCount == 0) Reset();
        }

        public void Reset()
        {
            OnPropertyChanged(CountProp);
            OnPropertyChanged(IndexerProp);
            OnCollectionChanged(new NotifyCollectionChangedEventArgs(NotifyCollectionChangedAction.Reset));
        }

        #region Private methods

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

        private void OnCollectionChanged(NotifyCollectionChangedEventArgs args)
        {
            CollectionChanged?.Invoke(this, args);
        }

        private void OnPropertyChanged(string propertyName)
        {
            PropertyChanged?.Invoke(this, new PropertyChangedEventArgs(propertyName));
        }

        protected virtual event PropertyChangedEventHandler PropertyChanged;

        private const string CountProp = "Count";
        private const string IndexerProp = "Item[]";

        #endregion
    }
}
