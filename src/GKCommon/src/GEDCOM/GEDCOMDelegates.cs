using System;

namespace GKCommon.GEDCOM
{
    public delegate GEDCOMTag TagConstructor(GEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue);

	public delegate void ProgressEventHandler(object sender, int progress);
}
