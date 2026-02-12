/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using System.Diagnostics;
using GKCore;
using GKCore.Design;
using GKCore.Design.Graphics;
using GKCore.Design.Views;
using GKCore.Utilities;
using GKCore.Validation;
using NSubstitute;

namespace GKTests.Stubs
{
    public sealed class AppHostStub : AppHost
    {
        static AppHostStub()
        {
            SetAppSign("GEDKeeperTest");
        }

        public AppHostStub()
        {
            Debug.WriteLine("AppHostStub.ctor()");
        }

        public override ITimer CreateTimer(double msInterval, EventHandler elapsedHandler)
        {
            return Substitute.For<ITimer>();
        }

        public override void Quit()
        {
        }

        public override bool ExecuteWork(ProgressStart proc, string title = "")
        {
            using (var progressForm = ResolveDialog<IProgressDialog>()) {
                proc(progressForm);
                Debug.WriteLine("AppHostStub.ExecuteWork()");
                return true;
            }
        }

        public override void Invoke(Action action)
        {
            action();
        }

        public static void ConfigureBootstrap()
        {
            var appHost = new AppHostStub();
            var container = AppHost.Container;

            if (container == null)
                throw new ArgumentNullException(nameof(container));

            container.Reset();
            ValidationFactory.InitGDMValidators();

            container.Register<IStdDialogs, StdDialogsStub>(LifeCycle.Singleton);
            container.Register<IGraphicsProvider, GfxProviderStub>(LifeCycle.Singleton);
        }
    }
}
