using BenchmarkDotNet.Attributes;
using GDModel;
using GDModel.Providers.GEDCOM;

namespace GKBenchmarks;

[Config(typeof(BenchmarkConfig))]
public class MediaFormatVal
{
    [Benchmark(Baseline = true)]
    public GDMMultimediaFormat GetMultimediaFormatVal_new()
    {
        return GEDCOMUtils.GetMultimediaFormatVal("xlsx");
    }

    [Benchmark]
    public GDMMultimediaFormat GetMultimediaFormatVal_old()
    {
        return GetMultimediaFormatVal_old("xlsx");
    }

    private static GDMMultimediaFormat GetMultimediaFormatVal_old(string str)
    {
        if (string.IsNullOrEmpty(str)) return GDMMultimediaFormat.mfNone;

        GDMMultimediaFormat result;
        str = str.Trim().ToLowerInvariant();

        if (str == "bmp") {
            result = GDMMultimediaFormat.mfBMP;
        } else if (str == "gif") {
            result = GDMMultimediaFormat.mfGIF;
        } else if (str == "jpg" || str == "jpeg") {
            result = GDMMultimediaFormat.mfJPG;
        } else if (str == "ole") {
            result = GDMMultimediaFormat.mfOLE;
        } else if (str == "pcx") {
            result = GDMMultimediaFormat.mfPCX;
        } else if (str == "tif" || str == "tiff") {
            result = GDMMultimediaFormat.mfTIF;
        } else if (str == "wav") {
            result = GDMMultimediaFormat.mfWAV;
        } else if (str == "txt") {
            result = GDMMultimediaFormat.mfTXT;
        } else if (str == "rtf") {
            result = GDMMultimediaFormat.mfRTF;
        } else if (str == "avi") {
            result = GDMMultimediaFormat.mfAVI;
        } else if (str == "tga") {
            result = GDMMultimediaFormat.mfTGA;
        } else if (str == "png") {
            result = GDMMultimediaFormat.mfPNG;
        } else if (str == "mpg" || str == "mpeg") {
            result = GDMMultimediaFormat.mfMPG;
        } else if (str == "htm" || str == "html") {
            result = GDMMultimediaFormat.mfHTM;
        } else if (str == "raw") {
            result = GDMMultimediaFormat.mfRAW;
        } else if (str == "mp3") {
            result = GDMMultimediaFormat.mfMP3;
        } else if (str == "wma") {
            result = GDMMultimediaFormat.mfWMA;
        } else if (str == "psd") {
            result = GDMMultimediaFormat.mfPSD;
        } else if (str == "pdf") {
            result = GDMMultimediaFormat.mfPDF;
        } else if (str == "mp4") {
            result = GDMMultimediaFormat.mfMP4;
        } else if (str == "ogv") {
            result = GDMMultimediaFormat.mfOGV;
        } else if (str == "mka") {
            result = GDMMultimediaFormat.mfMKA;
        } else if (str == "wmv") {
            result = GDMMultimediaFormat.mfWMV;
        } else if (str == "mkv") {
            result = GDMMultimediaFormat.mfMKV;
        } else if (str == "mov") {
            result = GDMMultimediaFormat.mfMOV;
        } else if (str == "djvu") {
            result = GDMMultimediaFormat.mfDJVU;
        } else if (str == "doc") {
            result = GDMMultimediaFormat.mfDOC;
        } else if (str == "docx") {
            result = GDMMultimediaFormat.mfDOCX;
        } else if (str == "xls") {
            result = GDMMultimediaFormat.mfXLS;
        } else if (str == "xlsx") {
            result = GDMMultimediaFormat.mfXLSX;
        } else if (str == "ppt") {
            result = GDMMultimediaFormat.mfPPT;
        } else if (str == "pptx") {
            result = GDMMultimediaFormat.mfPPTX;
        } else if (str == "odt") {
            result = GDMMultimediaFormat.mfODT;
        } else if (str == "ods") {
            result = GDMMultimediaFormat.mfODS;
        } else if (str == "odp") {
            result = GDMMultimediaFormat.mfODP;
        } else if (str == "zip") {
            result = GDMMultimediaFormat.mfZIP;
        } else if (str == "rar") {
            result = GDMMultimediaFormat.mfRAR;
        } else if (str == "7z") {
            result = GDMMultimediaFormat.mf7Z;
        } else if (str == "webp") {
            result = GDMMultimediaFormat.mfWEBP;
        } else {
            result = GDMMultimediaFormat.mfUnknown;
        }
        return result;
    }
}
