/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using BSLib;
using GKCore;
using GKCore.Design;
using GKCore.Design.Graphics;
using GKCore.Options;
using GKCore.Plugins;
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

    public override void Activate()
    {
    }

    public override IForm GetActiveForm()
    {
        return null;
    }

    public override IWindow GetActiveWindow()
    {
        return null;
    }

    public override nint GetTopWindowHandle()
    {
        return nint.Zero;
    }

    public override void EnableWindow(IWidgetForm form, bool value)
    {
    }

    public override void SaveWinState(IBaseWindow baseWin, MRUFile mf)
    {
    }

    public override void RestoreWinState(IBaseWindow baseWin, MRUFile mf)
    {
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

    public override void CloseDependentWindows(IWindow owner)
    {
    }

    public override ExtRect GetActiveScreenWorkingArea()
    {
        return ExtRect.Empty;
    }

    public override void SetWindowBounds(IWindow window, ExtRect bounds)
    {
    }

    public override void WidgetLocate(IWidgetForm view, WidgetLocation location)
    {
    }

    public override string SelectFolder(string folderPath)
    {
        return string.Empty;
    }

    public override void LayoutWindows(WinLayout layout)
    {
    }

    public override void Invoke(Action action)
    {
    }

    public override int GetKeyLayout()
    {
        return 0;
    }

    public override void SetKeyLayout(int layout)
    {
    }

    public override void SetClipboardText(string text)
    {
    }

    public override void SetClipboardImage(object image)
    {
    }

    public static void ConfigureBootstrap()
    {
        var appHost = new CLIAppHost();
        var container = Container;

        if (container == null)
            throw new ArgumentNullException(nameof(container));

        container.Reset();
        ValidationFactory.InitGDMValidators();

        //container.Register<IStdDialogs, StdDialogsStub>(LifeCycle.Singleton);
        container.Register<IGraphicsProvider, CLIGfxProvider>(LifeCycle.Singleton);
        //container.Register<IProgressDialog, ProgressDlg>(LifeCycle.Transient);
    }

    public static void Startup(string[] args)
    {
        ConfigureBootstrap();
        CheckPortable(args);
        Logger.Init(GetLogFilename());
        LogSysInfo();

        AppDomain.CurrentDomain.UnhandledException += UnhandledExceptionsHandler;
    }

    private static void UnhandledExceptionsHandler(object sender, UnhandledExceptionEventArgs e)
    {
        // Saving the copy for restoration
        Instance.CriticalSave();
        Logger.WriteError("GK.UnhandledExceptionsHandler()", (Exception)e.ExceptionObject);
    }
}
