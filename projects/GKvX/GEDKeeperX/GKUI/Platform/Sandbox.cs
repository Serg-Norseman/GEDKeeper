/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Runtime.CompilerServices;
using System.Threading;
using System.Threading.Tasks;
using GKCore.Design;
using Xamarin.Essentials;

namespace GKUI.Platform
{
    internal static class AsyncHelper
    {
        private static readonly TaskFactory _myTaskFactory =
            new TaskFactory(CancellationToken.None, TaskCreationOptions.None, TaskContinuationOptions.None, TaskScheduler.Default);

        public static TResult RunSync<TResult>(Func<Task<TResult>> func)
        {
            return _myTaskFactory.StartNew(func).Unwrap().GetAwaiter().GetResult();
        }

        public static void RunSync(Func<Task> func)
        {
            _myTaskFactory.StartNew(func).Unwrap().GetAwaiter().GetResult();
        }
    }


    public class BaseViewModel : INotifyPropertyChanged
    {
        private bool fIsBusy;
        private string fTitle;

        public bool IsBusy
        {
            get { return fIsBusy; }
            set { SetProperty(ref fIsBusy, value); }
        }

        public string Title
        {
            get { return fTitle; }
            set { SetProperty(ref fTitle, value); }
        }

        public BaseViewModel()
        {
            fTitle = string.Empty;
        }

        protected bool SetProperty<T>(ref T backingStore, T value,
            [CallerMemberName] string propertyName = "",
            Action onChanged = null)
        {
            if (EqualityComparer<T>.Default.Equals(backingStore, value))
                return false;

            backingStore = value;
            onChanged?.Invoke();
            OnPropertyChanged(propertyName);
            return true;
        }

        public event PropertyChangedEventHandler PropertyChanged;
        protected void OnPropertyChanged([CallerMemberName] string propertyName = "")
        {
            var changed = PropertyChanged;
            if (changed == null)
                return;

            changed.Invoke(this, new PropertyChangedEventArgs(propertyName));
        }
    }


    internal class ProgressControllerStub : IProgressController
    {
        public bool IsCanceled
        {
            get { return false; }
        }

        public void Begin(int maximum, bool cancelable)
        {
        }

        public void Begin(string title, int maximum, bool cancelable = false)
        {
        }

        public void End()
        {
        }

        public void End(ThreadError threadError)
        {
        }

        public void Increment(int value = 1)
        {
        }

        public void InvokeEx(Action action)
        {
        }

        public void SetText(string text)
        {
        }

        public void StepTo(int value)
        {
        }
    }


    public interface IDisplayChangeable
    {
        void OnDisplayChanged(DisplayInfo displayInfo);
    }
}
