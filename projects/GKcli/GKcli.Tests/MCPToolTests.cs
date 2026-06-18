using GKCore;
using GKCore.Locales;
using GKCortex.Features;

namespace GKcli.Tests;

public class MCPToolTests
{
    protected readonly BaseContext fContext;

    static MCPToolTests()
    {
        LangMan.DefInit();
        MCPController.InitFeatures(embedded: false, pureMode: false, tdeMode: true, ragMode: true);
    }

    public MCPToolTests()
    {
        fContext = new BaseContext(null);
    }
}
