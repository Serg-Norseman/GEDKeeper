/*
 *  This file is part of the "GKMap".
 *  GKMap project borrowed from GMap.NET (by radioman).
 *
 *  Copyright (C) 2009-2018 by radioman (email@radioman.lt).
 *  This program is licensed under the FLAT EARTH License.
 */

using System;
using System.ComponentModel;

namespace GKMap.WinForms
{
    public class ObservableCollectionThreadSafe<T> : ObservableCollection<T>
    {
        private NotifyCollectionChangedEventHandler fCollectionChanged;

        public override event NotifyCollectionChangedEventHandler CollectionChanged
        {
            add {
                fCollectionChanged += value;
            }
            remove {
                fCollectionChanged -= value;
            }
        }

        protected override void OnCollectionChanged(NotifyCollectionChangedEventArgs e)
        {
            // Be nice - use BlockReentrancy like MSDN said
            using (BlockReentrancy()) {
                if (fCollectionChanged != null) {
                    Delegate[] delegates = fCollectionChanged.GetInvocationList();

                    // Walk thru invocation list
                    foreach (var handler in delegates) {
                        var dispatcherObject = handler.Target as ISynchronizeInvoke /*System.Windows.Forms.Control*/;

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
    }
}
