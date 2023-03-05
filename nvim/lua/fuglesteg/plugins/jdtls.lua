return {
    "mfussenegger/nvim-jdtls",
    config = function()
        local config = {
            -- cmd = { 'jdtls', "-data", "/home/andreas/git/"},
            cmd = {
                               -- 💀
                'java', -- or '/path/to/java17_or_newer/bin/java'
                        -- depends on if `java` is in your $PATH env variable and if it points to the right version.

                '-Declipse.application=org.eclipse.jdt.ls.core.id1',
                '-Dosgi.bundles.defaultStartLevel=4',
                '-Declipse.product=org.eclipse.jdt.ls.core.product',
                '-Dlog.protocol=true',
                '-Dlog.level=ALL',
                '-Xms1g',
                '--add-modules=ALL-SYSTEM',
                '--add-opens', 'java.base/java.util=ALL-UNNAMED',
                '--add-opens', 'java.base/java.lang=ALL-UNNAMED',

                -- 💀
                '-jar', '/home/andreas/.local/share/nvim/mason/packages/jdtls/plugins/org.eclipse.equinox.launcher_1.6.400.v20210924-0641.jar',
                     -- ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^                                       ^^^^^^^^^^^^^^
                     -- Must point to the                                                     Change this to
                     -- eclipse.jdt.ls installation                                           the actual version


                -- 💀
                '-configuration', '/home/andreas/.local/share/nvim/mason/packages/jdtls/config_linux/',
                                -- ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^        ^^^^^^
                                -- Must point to the                      Change to one of `linux`, `win` or `mac`
                                -- eclipse.jdt.ls installation            Depending on your system.


                -- 💀
                -- See `data directory configuration` section in the README
                '-data', '/home/andreas/git/itx/'
            },
            root_dir = require("jdtls.setup").find_root({".git"}),
            init_options = {
                java = {
                    imports = {
                        gradle = {
                            wrapper = {
                                enabled = true,
                                checksums = {
                                    "e2b82129ab64751fd40437007bd2f7f2afb3c6e41a9198e628650b22d5824a14",
                                    "{'sha256': 'e2b82129ab64751fd40437007bd2f7f2afb3c6e41a9198e628650b22d5824a14', 'allowed': true}",
                                }
                            }
                        }
                    },
                }
            },
            settings = {
                java = {
                    configuration = {
                        runtimes = {
                            {
                                name = "JavaSE-19",
                                path = "/usr/lib/jvm/java-19-openjdk-amd64/",
                            },
                            {
                                name = "JavaSE-1.8",
                                path = "/usr/lib/jvm/java-8-openjdk-amd64"
                            },
                            {
                                name = "JavaSE-11",
                                path = "/usr/lib/jvm/java-11-openjdk-amd64/"
                            },
                        }
                    }
                }
            }
        }
        require('jdtls').start_or_attach(config)
    end,
    ft = "java",
}
