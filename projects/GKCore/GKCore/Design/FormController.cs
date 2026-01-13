/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using BSLib;

namespace GKCore.Design
{
    /// <summary>
    /// 
    /// </summary>
    public abstract class FormController<TView> : BaseObject where TView : IView
    {
        protected IBaseWindow fBase;
        protected readonly TView fView;


        public IBaseWindow Base
        {
            get { return fBase; }
        }


        protected FormController(TView view)
        {
            fView = view;
            SetLocale();
        }

        public virtual void Init(IBaseWindow baseWin)
        {
            fBase = baseWin;
        }

        public virtual void Done()
        {
        }

        protected T GetControl<T>(string controlName) where T : class, IControl
        {
            return fView.GetCoreControl<T>(controlName);
        }

        public virtual void UpdateView()
        {
            // dummy
        }

        public virtual void SetLocale()
        {
            // dummy
        }

        public virtual void ApplyTheme()
        {
            // dummy
        }

        protected virtual void SetToolTip(string componentName, string toolTip)
        {
            var ctl = fView.GetControl(componentName);
            fView.SetToolTip(ctl, toolTip);
        }
    }
}
