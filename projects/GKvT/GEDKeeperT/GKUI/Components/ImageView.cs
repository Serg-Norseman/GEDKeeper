/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System.Collections.Generic;
using GKCore.Design.Graphics;
using GKUI.Platform;
using GKUI.Platform.Handlers;
using SixLabors.ImageSharp;
using SixLabors.ImageSharp.PixelFormats;
using SixLabors.ImageSharp.Processing;
using Terminal.Gui;

namespace GKUI.Components
{
    using static GKUI.Platform.ColorMapper;
    using tgSize = Terminal.Gui.Size;
    using tgAttribute = Terminal.Gui.Attribute;

    public class ImageView : View
    {
        private static readonly List<PixColor> fPixColors = ColorMapper.InitPixelColors();

        private Dictionary<TrueColor, BrSym> fAttributeCache;
        private BrSym[,] fSymMatrix;
        private Image<Rgba32> fImage;
        private tgSize fImageTermSize;
        private Rect fOldSize = Rect.Empty;

        public ImageView()
        {
            fAttributeCache = new Dictionary<TrueColor, BrSym>();
        }

        public void OpenImage(object controller, IImage image)
        {
            fImage = ((ImageHandler)image).Handle;
            fAttributeCache.Clear();
            SetNeedsDisplay();
        }

        public override void Redraw(Rect bounds)
        {
            base.Redraw(bounds);

            if (fOldSize != bounds) {
                fOldSize = bounds;

                fImageTermSize = UIHelper.CalculateTerminalSize(fImage.Width, fImage.Height, bounds.Width, bounds.Height);
                var imgScaled = fImage.Clone(x => x.Resize(fImageTermSize.Width, fImageTermSize.Height, KnownResamplers.Robidoux));
                fSymMatrix = GenerateImageMatrix(imgScaled);
            }

            if (fSymMatrix != null) {
                int dx = (bounds.Width - fImageTermSize.Width) / 2;
                int dy = (bounds.Height - fImageTermSize.Height) / 2;

                for (int y = 0; y < fImageTermSize.Height; y++) {
                    for (int x = 0; x < fImageTermSize.Width; x++) {
                        var cell = fSymMatrix[y, x];
                        Driver.SetAttribute(cell.Attribute);
                        AddRune(dx + x, dy + y, cell.Symbol);
                    }
                }
            }
        }

        private record BrSym(char Symbol, tgAttribute Attribute);

        private BrSym[,] GenerateImageMatrix(Image<Rgba32> image)
        {
            var matrix = new BrSym[image.Height, image.Width];

            for (int y = 0; y < image.Height; y++) {
                for (int x = 0; x < image.Width; x++) {
                    Rgba32 pixel = image[x, y];
                    var trueColor = new TrueColor(pixel.R, pixel.G, pixel.B);

                    if (!fAttributeCache.TryGetValue(trueColor, out var brSym)) {
                        var pixClr = ColorMapper.FindBestMatch(fPixColors, pixel.R, pixel.G, pixel.B);
                        brSym = new BrSym(pixClr.chr, new tgAttribute(pixClr.fg, pixClr.bg));
                        fAttributeCache[trueColor] = brSym;
                    }

                    matrix[y, x] = new BrSym(brSym.Symbol, brSym.Attribute);
                }
            }

            return matrix;
        }
    }
}
