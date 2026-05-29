using System.Text.Json;
using GKcli.RAG;
using NUnit.Framework;

namespace GKcli.Tests;

[TestFixture]
public class RAGToolTests : MCPToolTests
{
    public RAGToolTests() : base()
    {
    }

    #region RAG Tools Tests

    [Test]
    public void Test_RAGSearchExamplesTool_ExecuteTool()
    {
        // Arrange
        var tool = new RAGSearchExamplesTool();
        JsonElement args = JsonSerializer.SerializeToElement(new {
            input_text = "John Smith, aged 35, farmer"
        });

        // Act
        var result = tool.ExecuteTool(fContext, args);

        // Assert
        Assert.IsNotNull(result);
        Assert.IsTrue(result.Count > 0);
        Assert.IsNotEmpty(result[0].Text);
    }

    [Test]
    public void Test_RAGSearchExamplesTool_ExecuteTool_WithAllParameters()
    {
        // Arrange
        var tool = new RAGSearchExamplesTool();
        JsonElement args = JsonSerializer.SerializeToElement(new {
            input_text = "John Smith, aged 35, farmer",
            century = "19",
            minScore = 0.7,
            topK = 5
        });

        // Act
        var result = tool.ExecuteTool(fContext, args);

        // Assert
        Assert.IsNotNull(result);
        Assert.IsTrue(result.Count > 0);
        Assert.IsNotEmpty(result[0].Text);
    }

    [Test]
    public void Test_RAGSearchExamplesTool_ExecuteTool_MissingInputText_ThrowsException()
    {
        // Arrange
        var tool = new RAGSearchExamplesTool();
        string jsonString = "{}";
        using var doc = JsonDocument.Parse(jsonString);
        JsonElement args = doc.RootElement;

        // Act & Assert
        Assert.Throws<ArgumentException>(() => tool.ExecuteTool(fContext, args));
    }

    [Test]
    public void Test_RAGWritePatternTool_ExecuteTool()
    {
        // Arrange
        var tool = new RAGWritePatternTool();
        JsonElement args = JsonSerializer.SerializeToElement(new {
            raw_text = "John Smith, aged 35, farmer",
            corrected_result = "{\"name\":\"John Smith\",\"age\":35,\"occupation\":\"farmer\"}",
            century = "19",
            quality_score = 0.9
        });

        // Act
        var result = tool.ExecuteTool(fContext, args);

        // Assert
        Assert.IsNotNull(result);
        Assert.IsTrue(result.Count > 0);
        Assert.IsTrue(result[0].Text.Contains("✅"));
    }

    [Test]
    public void Test_RAGWritePatternTool_ExecuteTool_MissingRequiredParameters_ThrowsException()
    {
        // Arrange
        var tool = new RAGWritePatternTool();
        string jsonString = "{}";
        using var doc = JsonDocument.Parse(jsonString);
        JsonElement args = doc.RootElement;

        // Act & Assert
        Assert.Throws<ArgumentException>(() => tool.ExecuteTool(fContext, args));
    }

    [Test]
    public void Test_RAGWritePatternTool_ExecuteTool_WithOptionalParameters()
    {
        // Arrange
        var tool = new RAGWritePatternTool();
        JsonElement args = JsonSerializer.SerializeToElement(new {
            raw_text = "John Smith, aged 35, farmer",
            corrected_result = "{\"name\":\"John Smith\",\"age\":35,\"occupation\":\"farmer\"}",
            century = "19"
        });

        // Act
        var result = tool.ExecuteTool(fContext, args);

        // Assert
        Assert.IsNotNull(result);
        Assert.IsTrue(result.Count > 0);
        Assert.IsTrue(result[0].Text.Contains("✅"));
    }

    #endregion
}
