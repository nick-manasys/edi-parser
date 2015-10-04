package com.anypoint.df.edi.schema.tools;

import java.io.StringReader;
import java.io.Writer;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.Map;

import org.yaml.snakeyaml.DumperOptions;
import org.yaml.snakeyaml.Yaml;
import org.yaml.snakeyaml.constructor.AbstractConstruct;
import org.yaml.snakeyaml.constructor.Constructor;
import org.yaml.snakeyaml.nodes.Node;
import org.yaml.snakeyaml.nodes.ScalarNode;
import org.yaml.snakeyaml.nodes.Tag;
import org.yaml.snakeyaml.representer.Represent;
import org.yaml.snakeyaml.representer.Representer;

public class YamlSupport
{
    public static void writeMap(Map<String, Object> map, Writer writer) {
        Yaml yaml = new Yaml(new EdiRepresenter(), new DumperOptions());
        yaml.dump(map, writer);
    }
    
    public static Map<String, Object> readMap(String text) {
        Yaml yaml = new Yaml(new EdiConstructor());
        return (Map<String, Object>)yaml.load(new StringReader(text));
    }
    
    private static class EdiRepresenter extends Representer
    {
        public EdiRepresenter() {
            representers.put(BigDecimal.class, new RepresentBigDecimal());
            representers.put(BigInteger.class, new RepresentBigInteger());
        }
        
        private class RepresentBigDecimal implements Represent {
            @Override
            public Node representData(Object value) {
                return representScalar(new Tag("!bigdecimal"), ((BigDecimal)value).toString());
            }
        }
        
        private class RepresentBigInteger implements Represent {
            @Override
            public Node representData(Object value) {
                return representScalar(new Tag("!biginteger"), ((BigInteger)value).toString());
            }
        }
    }
    
    private static class EdiConstructor extends Constructor
    {
        public EdiConstructor() {
            yamlConstructors.put(new Tag("!bigdecimal"), new ConstructBigDecimal());
            yamlConstructors.put(new Tag("!biginteger"), new ConstructBigInteger());
        }
        
        private class ConstructBigDecimal extends AbstractConstruct {
            @Override
            public Object construct(Node node) {
                String value = (String)constructScalar((ScalarNode)node);
                return new BigDecimal(value);
            }
        }
        
        private class ConstructBigInteger extends AbstractConstruct {
            @Override
            public Object construct(Node node) {
                String value = (String)constructScalar((ScalarNode)node);
                return new BigInteger(value);
            }
        }
    }
}
