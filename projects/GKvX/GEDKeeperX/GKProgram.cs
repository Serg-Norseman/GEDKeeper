/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using CommunityToolkit.Maui;
using GKUI;
using Microsoft.Maui.Controls.Hosting;
using Microsoft.Maui.Hosting;
using OxyPlot.Maui.Skia;
using SkiaSharp.Views.Maui.Controls.Hosting;

namespace GEDKeeperX;

public static class GKProgram
{
    public static MauiApp CreateMauiApp()
    {
        var builder = MauiApp.CreateBuilder();

        builder
            .UseMauiApp<App>()
            .UseMauiCommunityToolkit()
            // handlers for SKCanvasView (LogChart, ArborViewer, ScrollablePanel,
            // TreeMapViewer) and OxyPlot.PlotView (ZGraphControl) are not
            // registered by default; without them these windows crash on push
            .UseSkiaSharp()
            .UseOxyPlotSkia()
            .ConfigureFonts(fonts =>
        {
            fonts.AddFont("OpenSans-Regular.ttf", "OpenSansRegular");
        });

        return builder.Build();
    }
}
