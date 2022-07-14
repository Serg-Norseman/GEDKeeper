using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Runtime.CompilerServices;

namespace GKUI.Platform
{
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
            [CallerMemberName]string propertyName = "",
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
}
