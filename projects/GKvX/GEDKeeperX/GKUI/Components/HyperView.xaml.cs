/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System.Collections.Generic;
using System.Windows.Input;
using BSLib;
using GKCore;
using GKCore.BBText;
using GKCore.Design;
using GKCore.Design.Controls;
using GKCore.Utilities;
using Xamarin.Forms;

namespace GKUI.Components
{
    public delegate void LinkEventHandler(object sender, string linkName);

    /// <summary>
    ///
    /// </summary>
    public partial class HyperView : ScrollView, IHyperView
    {
        private readonly StringList fLines;
        private readonly ICommand fNavigationCommand;

        private Color fLinkColor;
        private bool fWordWrap;

        public event LinkEventHandler OnLink;

        public bool Enabled
        {
            get { return base.IsEnabled; }
            set { base.IsEnabled = value; }
        }

        public bool Visible
        {
            get { return base.IsVisible; }
            set { base.IsVisible = value; }
        }

        public StringList Lines
        {
            get { return fLines; }
        }

        public Color LinkColor
        {
            get { return fLinkColor; }
            set { fLinkColor = value; }
        }

        public bool WordWrap
        {
            get { return fWordWrap; }
            set { fWordWrap = value; }
        }


        public HyperView()
        {
            InitializeComponent();

            Orientation = ScrollOrientation.Both;
            Padding = 8;

            fLines = new StringList();
            fLines.OnChange += new NotifyEventHandler(LinesChanged);
            fLinkColor = Color.Blue;
            fWordWrap = true;

            fNavigationCommand = new Command<string>((url) => {
                var eventHandler = OnLink;
                eventHandler?.Invoke(this, url);
            });
        }

        public void Activate()
        {
            Focus();
        }

        private void LinesChanged(object sender)
        {
            var formattedString = new FormattedString();

            if (fLines.Count != 0) {
                var parser = new BBTextParser(AppHost.GfxProvider, AppHost.GfxProvider.GetDefaultFontSize(),
                                              new ColorHandler(fLinkColor), new ColorHandler(Color.Black));

                string text = SysUtils.StripHTML(fLines.Text);
                var chunks = new List<BBTextChunk>();
                parser.ParseText(chunks, text);

                int line = -1;
                int chunksCount = chunks.Count;
                int k = 0;
                while (k < chunksCount) {
                    BBTextChunk chunk = chunks[k];

                    if (line != chunk.Line) {
                        line = chunk.Line;

                        if (line != -1) {
                            var span = new Span();
                            span.Text = "\r\n";
                            formattedString.Spans.Add(span);
                        }
                    }

                    string chunkText = chunk.Text;
                    if (!string.IsNullOrEmpty(chunkText)) {
                        var span = new Span();
                        span.Text = chunkText;
                        span.TextColor = ((ColorHandler)chunk.Color).Handle;
                        span.FontSize = chunk.Size;

                        var chunkStyle = chunk.Style;
                        if (chunkStyle.HasFlag(GKFontStyle.Bold)) {
                            span.FontAttributes = FontAttributes.Bold;
                        } else if (chunkStyle.HasFlag(GKFontStyle.Italic)) {
                            span.FontAttributes = FontAttributes.Italic;
                        }
                        if (chunkStyle.HasFlag(GKFontStyle.Strikeout)) {
                            span.TextDecorations = TextDecorations.Strikethrough;
                        } else if (chunkStyle.HasFlag(GKFontStyle.Underline)) {
                            span.TextDecorations = TextDecorations.Underline;
                        }

                        if (!string.IsNullOrEmpty(chunk.URL)) {
                            var tapRecognizer = new TapGestureRecognizer();
                            tapRecognizer.Command = fNavigationCommand;
                            tapRecognizer.CommandParameter = chunk.URL;
                            span.GestureRecognizers.Add(tapRecognizer);
                            span.TextColor = Color.Blue;
                            span.TextDecorations = TextDecorations.Underline;
                        }

                        formattedString.Spans.Add(span);
                    }
                    k++;
                }
            }

            hvContent.FormattedText = formattedString;
        }
    }
}
