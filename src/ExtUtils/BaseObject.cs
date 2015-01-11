using System;

/// <summary>
/// 
/// </summary>

namespace ExtUtils
{
    public class BaseObject : IDisposable
	{
        private bool fDisposed;

        protected virtual void Dispose(bool disposing)
        {
        }

        public void Dispose()
        {
            if (!this.fDisposed)
            {
                this.Dispose(true /*called by user directly*/);
                this.fDisposed = true;
            }

            GC.SuppressFinalize(this);
        }

        ~BaseObject()
        {
            this.Dispose(false /*not called by user directly*/);
        }
    }
}
