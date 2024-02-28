/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2011-2024 by Sergey V. Zhdanovskih.
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

using System.Collections.Generic;
using System.Windows.Input;
using BSLib;
using GKCore;
using GKCore.BBText;
using GKCore.Design;
using GKCore.Design.Controls;
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
        private ICommand fNavigationCommand;

        private Color fLinkColor;
        private bool fWordWrap;

        public event LinkEventHandler OnLink;

        public bool Enabled
        {
            get { return base.IsEnabled; }
            set { base.IsEnabled = value; }
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
                        if (chunkStyle.HasFlag(BSDTypes.FontStyle.Bold)) {
                            span.FontAttributes = FontAttributes.Bold;
                        } else if (chunkStyle.HasFlag(BSDTypes.FontStyle.Italic)) {
                            span.FontAttributes = FontAttributes.Italic;
                        }
                        if (chunkStyle.HasFlag(BSDTypes.FontStyle.Strikeout)) {
                            span.TextDecorations = TextDecorations.Strikethrough;
                        } else if (chunkStyle.HasFlag(BSDTypes.FontStyle.Underline)) {
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
