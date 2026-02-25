/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using BSLib;
using GDModel;
using GKCore.Options;

namespace GKCore.Charts
{
    public enum ChartControlMode
    {
        Default,
        DragImage,
        ControlsVisible
    }

    public enum MouseAction
    {
        None,
        Select,
        Expand,
        Drag,
        Properties,
        Highlight,
        CollapseBranch,
        Info,
    }

    public enum MouseEvent
    {
        meDown,
        meMove,
        meUp
    }

    public interface ITreeLayout : IChartLayout
    {

    }

    public interface ITreeChart : IChart
    {
        int DepthLimitAncestors { get; set; }
        int DepthLimitDescendants { get; set; }
        int Height { get; set; }
        TreeChartKind Kind { get; set; }
        TreeChartModel Model { get; }
        TreeChartOptions Options { get; set; }
        float Scale { get; }
        TreeChartPerson Selected { get; set; }
        int Width { get; set; }

        void GenChart(bool rootCenter);
        void GenChart(GDMIndividualRecord iRec, TreeChartKind kind, bool rootCenter);
        ExtRect GetClientRect();
        ExtPoint GetDrawOrigin();
        ExtSize GetImageSize();
        ExtPoint GetOffsets();
        void Invalidate();
        void RefreshTree();
        void RenderImage(RenderTarget target, bool forciblyCentered = false);
        void SelectBy(TreeChartPerson person, bool needCenter);
        void SetRenderer(ChartRenderer renderer);
        void SetScale(float value);

        void ResetBackground();
    }
}
