/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2018 by Sergey V. Zhdanovskih.
 *
 *  This file is part of "GEDKeeper".
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

using System;
using System.Collections.Generic;
using BSLib;
using GKCore.Interfaces;
using GKCore.Operations;
using GKCore.UIContracts;

namespace GKCore.Controllers
{
    public interface IControlHandler
    {
    }

    public abstract class ControlHandler<T, TThis> : IControlHandler where TThis : ControlHandler<T, TThis>
    {
        protected T fControl;

        public virtual T Control
        {
            get { return fControl; }
        }

        protected ControlHandler(T control)
        {
            this.fControl = control;
        }
    }

    public interface ILabelHandler : IControlHandler
    {
        string Text { get; set; }
    }

    public interface IButtonHandler : IControlHandler
    {
        bool Enabled { get; set; }
        string Text { get; set; }
    }

    public interface ICheckBoxHandler : IControlHandler
    {
        bool Checked { get; set; }
        bool Enabled { get; set; }
        string Text { get; set; }
    }

    public interface IComboBoxHandler : IControlHandler
    {
        bool Enabled { get; set; }
        bool ReadOnly { get; set; }
        int SelectedIndex { get; set; }
        object SelectedItem { get; set; }
        object SelectedTag { get; set; }
        string Text { get; set; }

        void Add(object item);
        void AddItem(string caption, object tag, IImage image = null);
        void AddRange(object[] items, bool sorted = false);
        void AddStrings(StringList strings);
        void BeginUpdate();
        void Clear();
        void EndUpdate();
        void Select();
        void SortItems();
    }

    public interface ITextBoxHandler : IControlHandler
    {
        bool Enabled { get; set; }
        string[] Lines { get; set; }
        bool ReadOnly { get; set; }
        string Text { get; set; }

        void AppendText(string text);
        void Clear();
        void Select();
    }

    public interface INumericBoxHandler : IControlHandler
    {
        bool Enabled { get; set; }
        bool ReadOnly { get; set; }
        string Text { get; set; }
        double Value { get; set; }

        void Select();
    }

    public interface ITVNode
    {
    }

    public interface ITreeViewHandler : IControlHandler
    {
        bool Enabled { get; set; }

        ITVNode AddNode(ITVNode parent, string name, object tag);
        void BeginUpdate();
        void Clear();
        void EndUpdate();
        void Expand(ITVNode node);
    }

    public interface IProgressBarHandler : IControlHandler
    {
        int Minimum { get; set; }
        int Maximum { get; set; }
        int Value { get; set; }

        void Increment(int value);
    }

    public interface ILogChart : IControlHandler
    {
        void AddFragment(int val);
        void Clear();
    }


    public sealed class ControlsManager
    {
        private static readonly Dictionary<Type, Type> fHandlerTypes = new Dictionary<Type, Type>();

        private readonly Dictionary<object, IControlHandler> fHandlers = new Dictionary<object, IControlHandler>();

        public T GetControlHandler<T>(object control) where T : IControlHandler
        {
            IControlHandler handler;
            if (!fHandlers.TryGetValue(control, out handler)) {
                Type controlType = control.GetType();
                Type handlerType;
                if (fHandlerTypes.TryGetValue(controlType, out handlerType)) {
                    handler = (IControlHandler)Activator.CreateInstance(handlerType, control);
                } else {
                    throw new Exception("handler type not found");
                }
            }
            return (T)handler;
        }

        public static void RegisterHandlerType(Type controlType, Type handlerType)
        {
            if (fHandlerTypes.ContainsKey(controlType)) {
                fHandlerTypes.Remove(controlType);
            }
            fHandlerTypes.Add(controlType, handlerType);
        }
    }

    /// <summary>
    /// 
    /// </summary>
    public abstract class FormController<T> where T : IView
    {
        protected readonly T fView;
        protected IBaseWindow fBase;

        public IBaseWindow Base
        {
            get { return fBase; }
        }


        protected FormController(T view)
        {
            fView = view;
        }

        public virtual void Init(IBaseWindow baseWin)
        {
            fBase = baseWin;
        }

        public abstract void UpdateView();
    }

    /// <summary>
    /// 
    /// </summary>
    public abstract class DialogController<T> : FormController<T> where T : IView
    {
        protected ChangeTracker fLocalUndoman;

        public ChangeTracker LocalUndoman
        {
            get { return fLocalUndoman; }
        }

        protected DialogController(T view) : base(view)
        {
        }

        public virtual bool Accept()
        {
            return true;
        }

        public virtual void Cancel()
        {
            RollbackChanges();
        }

        protected void CommitChanges()
        {
            fLocalUndoman.Commit();
        }

        protected void RollbackChanges()
        {
            fLocalUndoman.Rollback();
        }

        public override void Init(IBaseWindow baseWin)
        {
            base.Init(baseWin);
            if (fBase != null) {
                fLocalUndoman = new ChangeTracker(fBase.Context.Tree);
            }
        }
    }
}
