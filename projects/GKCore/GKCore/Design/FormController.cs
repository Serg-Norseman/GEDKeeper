/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using System.Collections.Generic;
using System.Reflection;
using BSLib;

namespace GKCore.Design
{
    [AttributeUsage(AttributeTargets.Field)]
    public class ControlAttribute : Attribute
    {
        public string Name { get; }
        public ControlAttribute(string name) => Name = name;
    }


    public class ControlsManager : BaseObject
    {
        protected static readonly Dictionary<Type, Type> fHandlerTypes = new Dictionary<Type, Type>();

        public static void RegisterHandlerType(Type controlType, Type handlerType)
        {
            if (fHandlerTypes.ContainsKey(controlType)) {
                fHandlerTypes.Remove(controlType);
            }
            fHandlerTypes.Add(controlType, handlerType);
        }

        public static void ClearHandlerTypes()
        {
            fHandlerTypes.Clear();
        }
    }


    /// <summary>
    /// 
    /// </summary>
    public abstract class FormController<TView> : ControlsManager, IViewController where TView : IView
    {
        protected IBaseWindow fBase;
        protected readonly TView fView;


        public IBaseWindow Base
        {
            get { return fBase; }
        }


        protected FormController(TView view) : base()
        {
            fView = view;
            fObjHandlers = new Dictionary<object, IControl>();
            fStrHandlers = new Dictionary<string, IControl>();
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing) {
                fObjHandlers.Clear();
                fStrHandlers.Clear();
            }
            base.Dispose(disposing);
        }

        private static IEnumerable<FieldInfo> GetAllFields(Type type)
        {
            while (type != null && type != typeof(ControlsManager) && type != typeof(object)) {
                var fields = type.GetFields(BindingFlags.Instance | BindingFlags.NonPublic/* | BindingFlags.Public*/ | BindingFlags.DeclaredOnly);

                foreach (var field in fields) {
                    yield return field;
                }

                type = type.BaseType;
            }
        }

        protected void BindControls()
        {
            /*
             * Looking ahead: if forms are opened frequently and in large numbers, the result of `GetFields` can be cached
             * in a `static ConcurrentDictionary<Type, FieldInfo[]>` — this will reduce the overhead to the microseconds.
             */
            var fields = GetAllFields(GetType());

            foreach (var field in fields) {
                if (!typeof(IControl).IsAssignableFrom(field.FieldType))
                    continue;

                string fieldName = field.Name;

                if (!fieldName.StartsWith("_"))
                    continue;

                var attr = field.GetCustomAttribute<ControlAttribute>();
                string controlName = attr?.Name ?? fieldName.TrimStart('_');

                var controlInstance = GetControl(controlName, field.FieldType);
                field.SetValue(this, controlInstance);
            }
        }

        public virtual void InitView(IBaseWindow baseWin)
        {
            BindControls();
            SetLocale();

            fBase = baseWin;
        }

        public virtual void DoneView()
        {
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

        #region ControlsManager

        private readonly Dictionary<object, IControl> fObjHandlers;
        private readonly Dictionary<string, IControl> fStrHandlers;

        public IControl GetControlHandler(object control, Type targetType)
        {
            if (control == null)
                throw new ArgumentNullException(nameof(control));
            if (targetType == null)
                throw new ArgumentNullException(nameof(targetType));

            if (targetType.IsInstanceOfType(control))
                return (IControl)control;

            if (!fObjHandlers.TryGetValue(control, out IControl handler)) {
                Type controlType = control.GetType();
                if (fHandlerTypes.TryGetValue(controlType, out Type handlerType)) {
                    handler = (IControl)Activator.CreateInstance(handlerType, control);
                    fObjHandlers.Add(control, handler);
                } else {
                    throw new Exception($"Handler type for `{controlType.Name}` not found");
                }
            }

            if (!targetType.IsInstanceOfType(handler))
                throw new InvalidCastException($"Cannot cast handler `{handler.GetType().Name}` to `{targetType.Name}`");

            return handler;
        }

        public IControl GetControl(string controlName, Type targetType)
        {
            if (targetType == null)
                throw new ArgumentNullException(nameof(targetType));

            if (!fStrHandlers.TryGetValue(controlName, out IControl control)) {
                if (fView == null)
                    throw new ArgumentException("View is null");

                var ctlObj = fView.GetControl(controlName);

                if (ctlObj == null)
                    throw new ArgumentException($"Field `{controlName}` not found");

                control = GetControlHandler(ctlObj, targetType);
                fStrHandlers.Add(controlName, control);
            } else if (!targetType.IsInstanceOfType(control)) {
                throw new InvalidCastException($"Cached control `{controlName}` cannot be cast to `{targetType.Name}`");
            }

            return control;
        }

        public T GetControlHandler<T>(object control) where T : class, IControl
        {
            return (T)GetControlHandler(control, typeof(T));
        }

        public T GetControl<T>(string controlName) where T : class, IControl
        {
            return (T)GetControl(controlName, typeof(T));
        }


        /*public T GetControlHandler<T>(object control) where T : class, IControl
        {
            if (control is T ctlT)
                return ctlT;

            IControl handler;
            if (!fObjHandlers.TryGetValue(control, out handler)) {
                Type controlType = control.GetType();
                Type handlerType;
                if (fHandlerTypes.TryGetValue(controlType, out handlerType)) {
                    handler = (IControl)Activator.CreateInstance(handlerType, control);
                    fObjHandlers.Add(control, handler);
                } else {
                    throw new Exception(string.Format("handler type for `{0}` not found", controlType.Name));
                }
            }
            return (T)handler;
        }

        public T GetControl<T>(string controlName) where T : class, IControl
        {
            IControl control;
            if (!fStrHandlers.TryGetValue(controlName, out control)) {
                if (fView == null)
                    throw new ArgumentException("View is null");

                var ctlObj = fView.GetControl(controlName);

                if (ctlObj == null)
                    throw new ArgumentException($"Field `{controlName}` not found");

                control = GetControlHandler<T>(ctlObj);
                fStrHandlers.Add(controlName, control);
            }
            return (T)control;
        }*/

        #endregion
    }
}
