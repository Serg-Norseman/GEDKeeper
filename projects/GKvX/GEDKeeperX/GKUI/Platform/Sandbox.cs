using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Runtime.CompilerServices;
using System.Threading;
using System.Threading.Tasks;
using GKCore;
using GKCore.Interfaces;
using Xamarin.Forms;
using Xamarin.Forms.Xaml;

namespace GKUI.Platform
{
    [ContentProperty("Source")]
    public class ImageResourceExt : IMarkupExtension
    {
        public string Source { get; set; }

        public object ProvideValue(IServiceProvider serviceProvider)
        {
            if (Source == null) {
                return null;
            }

            string resName = Source;//Application.Current.Resources[Source] as string;
            var imageSource = ImageSource.FromResource(resName, typeof(GKUtils).Assembly);
            return imageSource;
        }
    }


    internal static class AsyncHelper
    {
        private static readonly TaskFactory _myTaskFactory = new
          TaskFactory(CancellationToken.None,
                      TaskCreationOptions.None,
                      TaskContinuationOptions.None,
                      TaskScheduler.Default);

        public static TResult RunSync<TResult>(Func<Task<TResult>> func)
        {
            return AsyncHelper._myTaskFactory
              .StartNew<Task<TResult>>(func)
              .Unwrap<TResult>()
              .GetAwaiter()
              .GetResult();
        }

        public static void RunSync(Func<Task> func)
        {
            AsyncHelper._myTaskFactory
              .StartNew<Task>(func)
              .Unwrap()
              .GetAwaiter()
              .GetResult();
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

        #region INotifyPropertyChanged
        public event PropertyChangedEventHandler PropertyChanged;
        protected void OnPropertyChanged([CallerMemberName] string propertyName = "")
        {
            var changed = PropertyChanged;
            if (changed == null)
                return;

            changed.Invoke(this, new PropertyChangedEventArgs(propertyName));
        }
        #endregion
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
}
