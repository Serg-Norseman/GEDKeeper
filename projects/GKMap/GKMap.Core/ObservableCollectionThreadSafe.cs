/*
 *  This file is part of the "GKMap".
 *  GKMap project borrowed from GMap.NET (by radioman).
 *
 *  Copyright (C) 2009-2018 by radioman (email@radioman.lt).
 *  This program is licensed under the FLAT EARTH License.
 */

using System;
using System.Collections;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.ComponentModel;

namespace GKMap
{
    public delegate void NotifyCollectionChangedEventHandler(object sender, NotifyCollectionChangedEventArgs e);

    public interface INotifyCollectionChanged
    {
        event NotifyCollectionChangedEventHandler CollectionChanged;
    }

    public interface INotifyPropertyChanged
    {
        event PropertyChangedEventHandler PropertyChanged;
    }

    public enum NotifyCollectionChangedAction
    {
        Add,
        Remove,
        Replace,
        Move,
        Reset
    }

    public class NotifyCollectionChangedEventArgs : EventArgs
    {
        private NotifyCollectionChangedAction fAction;
        private IList fNewItems;
        private int fNewStartingIndex;
        private IList fOldItems;
        private int fOldStartingIndex;

        public NotifyCollectionChangedEventArgs(NotifyCollectionChangedAction action)
        {
            fNewStartingIndex = -1;
            fOldStartingIndex = -1;
            if (action != NotifyCollectionChangedAction.Reset) {
                throw new ArgumentException("WrongActionForCtor", "action");
            }
            InitializeAdd(action, null, -1);
        }

        public NotifyCollectionChangedEventArgs(NotifyCollectionChangedAction action, object changedItem, int index)
        {
            fNewStartingIndex = -1;
            fOldStartingIndex = -1;
            if (((action != NotifyCollectionChangedAction.Add) && (action != NotifyCollectionChangedAction.Remove)) && (action != NotifyCollectionChangedAction.Reset)) {
                throw new ArgumentException("MustBeResetAddOrRemoveActionForCtor", "action");
            }
            if (action == NotifyCollectionChangedAction.Reset) {
                if (changedItem != null) {
                    throw new ArgumentException("ResetActionRequiresNullItem", "action");
                }
                if (index != -1) {
                    throw new ArgumentException("ResetActionRequiresIndexMinus1", "action");
                }
                InitializeAdd(action, null, -1);
            } else {
                InitializeAddOrRemove(action, new object[] { changedItem }, index);
            }
        }

        public NotifyCollectionChangedEventArgs(NotifyCollectionChangedAction action, object changedItem, int index, int oldIndex)
        {
            fNewStartingIndex = -1;
            fOldStartingIndex = -1;
            if (action != NotifyCollectionChangedAction.Move) {
                throw new ArgumentException("WrongActionForCtor", "action");
            }
            if (index < 0) {
                throw new ArgumentException("IndexCannotBeNegative", "index");
            }
            object[] newItems = new object[] { changedItem };
            InitializeMoveOrReplace(action, newItems, newItems, index, oldIndex);
        }

        public NotifyCollectionChangedEventArgs(NotifyCollectionChangedAction action, object newItem, object oldItem, int index)
        {
            fNewStartingIndex = -1;
            fOldStartingIndex = -1;
            if (action != NotifyCollectionChangedAction.Replace) {
                throw new ArgumentException("WrongActionForCtor", "action");
            }
            InitializeMoveOrReplace(action, new object[] { newItem }, new object[] { oldItem }, index, index);
        }

        private void InitializeAdd(NotifyCollectionChangedAction action, IList newItems, int newStartingIndex)
        {
            fAction = action;
            fNewItems = (newItems == null) ? null : ArrayList.ReadOnly(newItems);
            fNewStartingIndex = newStartingIndex;
        }

        private void InitializeAddOrRemove(NotifyCollectionChangedAction action, IList changedItems, int startingIndex)
        {
            if (action == NotifyCollectionChangedAction.Add) {
                InitializeAdd(action, changedItems, startingIndex);
            } else if (action == NotifyCollectionChangedAction.Remove) {
                InitializeRemove(action, changedItems, startingIndex);
            } else {
                throw new ArgumentException(string.Format("InvariantFailure, Unsupported action: {0}", action.ToString()));
            }
        }

        private void InitializeMoveOrReplace(NotifyCollectionChangedAction action, IList newItems, IList oldItems, int startingIndex, int oldStartingIndex)
        {
            InitializeAdd(action, newItems, startingIndex);
            InitializeRemove(action, oldItems, oldStartingIndex);
        }

        private void InitializeRemove(NotifyCollectionChangedAction action, IList oldItems, int oldStartingIndex)
        {
            fAction = action;
            fOldItems = (oldItems == null) ? null : ArrayList.ReadOnly(oldItems);
            fOldStartingIndex = oldStartingIndex;
        }

        public NotifyCollectionChangedAction Action
        {
            get {
                return fAction;
            }
        }

        public IList NewItems
        {
            get {
                return fNewItems;
            }
        }

        public int NewStartingIndex
        {
            get {
                return fNewStartingIndex;
            }
        }

        public IList OldItems
        {
            get {
                return fOldItems;
            }
        }

        public int OldStartingIndex
        {
            get {
                return fOldStartingIndex;
            }
        }
    }


    public class ObservableCollectionThreadSafe<T> : Collection<T>, INotifyCollectionChanged, INotifyPropertyChanged
    {
        private readonly SimpleMonitor fMonitor;
        private const string CountString = "Count";
        private const string IndexerName = "Item[]";

        private NotifyCollectionChangedEventHandler fCollectionChanged;

        public event NotifyCollectionChangedEventHandler CollectionChanged
        {
            add {
                fCollectionChanged += value;
            }
            remove {
                fCollectionChanged -= value;
            }
        }

        protected event PropertyChangedEventHandler PropertyChanged;

        event PropertyChangedEventHandler INotifyPropertyChanged.PropertyChanged
        {
            add {
                PropertyChanged += value;
            }
            remove {
                PropertyChanged -= value;
            }
        }

        public ObservableCollectionThreadSafe()
        {
            fMonitor = new SimpleMonitor();
        }

        public ObservableCollectionThreadSafe(IEnumerable<T> collection)
        {
            fMonitor = new SimpleMonitor();
            if (collection == null) {
                throw new ArgumentNullException("collection");
            }
            CopyFrom(collection);
        }

        public ObservableCollectionThreadSafe(List<T> list)
           : base((list != null) ? new List<T>(list.Count) : list)
        {
            fMonitor = new SimpleMonitor();
            CopyFrom(list);
        }

        protected IDisposable BlockReentrancy()
        {
            fMonitor.Enter();
            return fMonitor;
        }

        protected void CheckReentrancy()
        {
            if ((fMonitor.Busy && (fCollectionChanged != null)) && (fCollectionChanged.GetInvocationList().Length > 1)) {
                throw new InvalidOperationException("ObservableCollectionReentrancyNotAllowed");
            }
        }

        protected override void ClearItems()
        {
            CheckReentrancy();
            base.ClearItems();
            OnPropertyChanged(CountString);
            OnPropertyChanged(IndexerName);
            OnCollectionReset();
        }

        private void CopyFrom(IEnumerable<T> collection)
        {
            IList<T> items = Items;
            if (collection != null) {
                using (IEnumerator<T> enumerator = collection.GetEnumerator()) {
                    while (enumerator.MoveNext()) {
                        items.Add(enumerator.Current);
                    }
                }
            }
        }

        protected override void InsertItem(int index, T item)
        {
            CheckReentrancy();
            base.InsertItem(index, item);
            OnPropertyChanged(CountString);
            OnPropertyChanged(IndexerName);
            OnCollectionChanged(NotifyCollectionChangedAction.Add, item, index);
        }

        public void Move(int oldIndex, int newIndex)
        {
            MoveItem(oldIndex, newIndex);
        }

        protected virtual void MoveItem(int oldIndex, int newIndex)
        {
            CheckReentrancy();
            T item = base[oldIndex];
            base.RemoveItem(oldIndex);
            base.InsertItem(newIndex, item);
            OnPropertyChanged(IndexerName);
            OnCollectionChanged(NotifyCollectionChangedAction.Move, item, newIndex, oldIndex);
        }

        protected void OnCollectionChanged(NotifyCollectionChangedEventArgs e)
        {
            // Be nice - use BlockReentrancy like MSDN said
            using (BlockReentrancy()) {
                if (fCollectionChanged != null) {
                    Delegate[] delegates = fCollectionChanged.GetInvocationList();

                    // Walk thru invocation list
                    foreach (var handler in delegates) {
                        var dispatcherObject = handler.Target as ISynchronizeInvoke /*SysWinForms.Control*/;

                        // If the subscriber is a DispatcherObject and different thread
                        if (dispatcherObject != null && dispatcherObject.InvokeRequired) {
                            // Invoke handler in the target dispatcher's thread
                            dispatcherObject.Invoke(handler, new object[] { this, e });
                        } else {
                            // Execute handler as is
                            fCollectionChanged(this, e);
                        }
                    }
                }
            }
        }

        private void OnCollectionChanged(NotifyCollectionChangedAction action, object item, int index)
        {
            OnCollectionChanged(new NotifyCollectionChangedEventArgs(action, item, index));
        }

        private void OnCollectionChanged(NotifyCollectionChangedAction action, object item, int index, int oldIndex)
        {
            OnCollectionChanged(new NotifyCollectionChangedEventArgs(action, item, index, oldIndex));
        }

        private void OnCollectionChanged(NotifyCollectionChangedAction action, object oldItem, object newItem, int index)
        {
            OnCollectionChanged(new NotifyCollectionChangedEventArgs(action, newItem, oldItem, index));
        }

        private void OnCollectionReset()
        {
            OnCollectionChanged(new NotifyCollectionChangedEventArgs(NotifyCollectionChangedAction.Reset));
        }

        protected virtual void OnPropertyChanged(PropertyChangedEventArgs e)
        {
            PropertyChanged?.Invoke(this, e);
        }

        private void OnPropertyChanged(string propertyName)
        {
            OnPropertyChanged(new PropertyChangedEventArgs(propertyName));
        }

        protected override void RemoveItem(int index)
        {
            CheckReentrancy();
            T item = base[index];
            base.RemoveItem(index);
            OnPropertyChanged(CountString);
            OnPropertyChanged(IndexerName);
            OnCollectionChanged(NotifyCollectionChangedAction.Remove, item, index);
        }

        protected override void SetItem(int index, T item)
        {
            CheckReentrancy();
            T oldItem = base[index];
            base.SetItem(index, item);
            OnPropertyChanged(IndexerName);
            OnCollectionChanged(NotifyCollectionChangedAction.Replace, oldItem, item, index);
        }


        private class SimpleMonitor : IDisposable
        {
            private int fBusyCount;

            public void Dispose()
            {
                fBusyCount--;
            }

            public void Enter()
            {
                fBusyCount++;
            }

            public bool Busy
            {
                get {
                    return (fBusyCount > 0);
                }
            }
        }
    }
}
