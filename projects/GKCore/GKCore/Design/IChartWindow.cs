/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

namespace GKCore.Design
{
    public enum ChartWindowsShowMode
    {
        Default,
        Maximize,
        LeftHalf,
        RightHalf,
    }


    /// <summary>
    /// Interface for windows of different charts with support
    /// for working functions and localization, and printing.
    /// </summary>
    public interface IChartWindow : IWorkWindow, IWindowDependent
    {
        bool AllowPrint();
        void DoPrint();
        void DoPrintPreview();
        void GenChart();
    }
}
