using System;
using Eto;
using Eto.Forms;

namespace GKUI.Platform
{
    [Handler(typeof(NativeHostControl.IHandler))]
    public class NativeHostControl : Control
    {
        new IHandler Handler { get { return (IHandler)base.Handler; } }

        public new IntPtr NativeHandle => Handler.NativeHandle;

        public NativeHostControl() : base()
        {
        }

        [AutoInitialize(false)]
        public new interface IHandler : Control.IHandler
        {
            bool IsInitialized { get; }
        }

        public new interface ICallback : Control.ICallback
        {
        }

        protected new class Callback : Control.Callback, ICallback
        {
        }

        //Gets an instance of an object used to perform callbacks to the widget from handler implementations

        static readonly object callback = new Callback();

        protected override object GetCallback()
        {
            return callback;
        }

        public bool IsInitialized
        {
            get { return Handler.IsInitialized; }
        }
    }
}
