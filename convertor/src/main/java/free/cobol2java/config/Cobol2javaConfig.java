package free.cobol2java.config;

import com.typesafe.config.Config;
import com.typesafe.config.ConfigException;
import com.typesafe.config.ConfigFactory;
import free.servpp.config.IConfigurable;
import free.servpp.config.IConfigurableForLoad;
import free.servpp.config.hocon.HoconConfigLoader;
import free.servpp.config.hocon.HoconConfigTypeManager;
import free.servpp.config.hocon.IConfigurableBuilder;

import java.io.IOException;
import java.io.StringReader;
import java.util.List;

/**
 * @author lidong@date 2024-09-10@version 1.0
 */
public class Cobol2javaConfig {
    HoconConfigLoader loader;
    HoconConfigTypeManager manager;
    public Cobol2javaConfig() throws IOException {
        HoconConfigLoader loader = new HoconConfigLoader(".conf", "", new IConfigurableBuilder() {
            @Override
            public IConfigurable build(String type, Config config) {
                if (type.equals("application")) {
                    return new IConfigurableForLoad() {
                        @Override
                        public String getConfigId() {
                            try {
                                return config.getString("application.name");
                            }catch (ConfigException.Missing e){
                                return null;
                            }
                        }

                        @Override
                        public String getModifier() {
                            return null;
                        }

                        @Override
                        public List<String> getConfigIdList() {
                            try {
                                return config.getStringList("application.name");
                            } catch (ConfigException.WrongType | ConfigException.Missing t) {
                                return null;
                            }
                        }

                        @Override
                        public List<String> getModifierList() {
                            return null;
                        }

                        @Override
                        public String getConfigType() {
                            return type;
                        }

                        @Override
                        public Config createConfigById(String id) {
                            String s = "application{name=" + id + "}";
                            return ConfigFactory.parseReader(new StringReader(s));
                        }

                        @Override
                        public Config createConfigByModifier(String modifier) {
                            String s = "application{modifier=" + modifier + "}";
                            return ConfigFactory.parseReader(new StringReader(s));
                        }
                    };
                }
                return null;
            }
        });
        manager = loader.getManager();
    }

    public HoconConfigLoader getLoader() {
        return loader;
    }

    public HoconConfigTypeManager getManager() {
        return manager;
    }
}
