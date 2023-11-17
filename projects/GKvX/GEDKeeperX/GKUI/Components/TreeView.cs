using System;
using GKCore.Design.Controls;
using Xamarin.Forms;

namespace GKUI.Components
{
    /// <summary>
    /// 
    /// </summary>
    public class GKTreeNode : ITVNode
    {
        public object Tag { get; set; }

        public string Text { get; set; }

        public GKTreeNode(string text, object tag) : base()
        {
            Text = text;
            Tag = tag;
        }
    }

    public class TreeView : ContentView
    {
        public event EventHandler MouseDoubleClick;
    }
}
