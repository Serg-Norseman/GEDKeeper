/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2023 by Sergey V. Zhdanovskih.
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

using BSLib;
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
        //private ICommand _navigationCommand;

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

            /*_navigationCommand = new Command<string>((url) => {
                var eventHandler = OnLink;
                eventHandler?.Invoke(this, url);
            });*/

            fLines = new StringList();
            fLines.OnChange += LinesChanged;
            fWordWrap = true;
        }

        public void Activate()
        {
            Focus();
        }

        private void LinesChanged(object sender)
        {
            string str = fLines.Text;
            str = str.
                Replace("[b]", "<b>").Replace("[/b]", "</b>").
                Replace("[u]", "<u>").Replace("[/u]", "</u>").
                Replace("[s]", "<s>").Replace("[/s]", "</s>").
                Replace("[i]", "<i>").Replace("[/i]", "</i>").
                Replace("\r\n", "<br>").
                Replace("[/url]", "</a>").Replace("[url=", "<a href='").
                Replace("[/size]", "</font>").Replace("[size=", "<font size='").
                Replace("]", "'>");

            //hvContent.FormattedText = Convert(str);
            hvContent.Text = str;
        }

        /*private FormattedString Convert(string value)
        {
            var formatted = new FormattedString();

            foreach (var item in ProcessString(value))
                formatted.Spans.Add(CreateSpan(item));

            return formatted;
        }

        private Span CreateSpan(StringSection section)
        {
            var span = new Span() {
                Text = section.Text
            };

            if (!string.IsNullOrEmpty(section.Link)) {
                span.GestureRecognizers.Add(new TapGestureRecognizer() {
                    Command = _navigationCommand,
                    CommandParameter = section.Link
                });
                span.TextColor = Color.Blue;
            }

            return span;
        }

        public IList<StringSection> ProcessString(string rawText)
        {
            const string spanPattern = @"(<a.*?>.*?</a>)";

            MatchCollection collection = Regex.Matches(rawText, spanPattern, RegexOptions.Singleline);

            var sections = new List<StringSection>();

            var lastIndex = 0;

            foreach (Match item in collection) {
                var foundText = item.Value;
                try {
                    var prevText = rawText.Substring(lastIndex, item.Index - lastIndex);
                    sections.Add(new StringSection() { Text = prevText });
                } catch (Exception ex) {
                    Logger.WriteError("", ex);
                }
                
                lastIndex = item.Index + item.Length;

                // Get HTML href 
                var html = new StringSection() {
                    Link = Regex.Match(item.Value, "(?<=href=\\\')[\\S]+(?=\\\')").Value,
                    Text = Regex.Replace(item.Value, "<.*?>", string.Empty)
                };

                sections.Add(html);
            }

            sections.Add(new StringSection() { Text = rawText.Substring(lastIndex) });

            return sections;
        }

        public class StringSection
        {
            public string Text { get; set; }
            public string Link { get; set; }
        }*/
    }
}
