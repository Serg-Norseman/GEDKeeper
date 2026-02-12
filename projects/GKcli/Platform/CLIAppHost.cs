/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using GKCore;
using GKCore.Design;
using GKCore.Design.Graphics;
using GKCore.Utilities;
using GKCore.Validation;

namespace GKUI.Platform;

public sealed class CLIAppHost : AppHost
{
    static CLIAppHost()
    {
        SetAppSign("GKcli");
    }

    public CLIAppHost()
    {
    }

    protected override void UpdateLang()
    {
        try {
            CLILangMan.Instance = this.CreateLangMan(this);
        } catch (Exception ex) {
            Logger.WriteError("CLIAppHost.UpdateLang()", ex);
        }
    }

    public override ITimer CreateTimer(double msInterval, EventHandler elapsedHandler)
    {
        return new CLITimer(msInterval, elapsedHandler);
    }

    public override void Quit()
    {
    }

    public override bool ExecuteWork(ProgressStart proc, string title = "")
    {
        using (var progressForm = ResolveDialog<IProgressDialog>()) {
            proc(progressForm);
            return true;
        }
    }

    public override void Invoke(Action action)
    {
        action();
    }

    public static void Startup(string[] args)
    {
        var appHost = new CLIAppHost();

        var container = Container;
        container.Reset();
        ValidationFactory.InitGDMValidators();
        container.Register<IGraphicsProvider, CLIGfxProvider>(LifeCycle.Singleton);
        container.Register<IProgressDialog, CLIProgress>(LifeCycle.Transient);

        CheckPortable(args);
        Logger.Init(GetLogFilename());
        LogSysInfo();
    }
}
