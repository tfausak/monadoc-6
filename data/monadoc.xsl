<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
    <xsl:output
        doctype-system="about:legacy-compat"
        encoding="UTF-8"
        media-type="text/html"
        method="html"/>

    <xsl:variable name="base-url" select="normalize-space(/monadoc/config/base-url)"/>
    <xsl:variable name="client-id" select="normalize-space(/monadoc/config/client-id)"/>
    <xsl:variable name="user" select="normalize-space(/monadoc/user/login)"/>
    <xsl:variable name="version" select="normalize-space(/monadoc/config/version)"/>

    <xsl:template match="user">
        <xsl:choose>
            <xsl:when test="$user">
                @<xsl:value-of select="$user"/>
            </xsl:when>
            <xsl:otherwise>
                <a class="nav-link" href="https://github.com/login/oauth/authorize?client_id={$client-id}&amp;redirect_uri={$base-url}/github-callback">Log in</a>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>

    <xsl:template match="/monadoc">
        <html lang="en-US">
            <head>
                <meta content="initial-scale = 1, width = device-width" name="viewport"/>
                <title>
                    Monadoc
                </title>
                <link href="{$base-url}/bootstrap.css" rel="stylesheet"/>
                <link
                    href="{$base-url}/monadoc.svg"
                    rel="icon"
                    type="image/svg+xml"/>
            </head>
            <body>
                <header class="mb-3">
                    <nav class="navbar navbar-light bg-light">
                        <div class="container-fluid">
                            <a class="navbar-brand" href="{$base-url}/">Monadoc</a>
                            <ul class="navbar-nav">
                                <li class="nav-item">
                                    <xsl:apply-templates select="user"/>
                                </li>
                            </ul>
                        </div>
                    </nav>
                </header>
                <main class="container-fluid mt-3 mb-3">
                    <p>
                        &#x1f516; Better Haskell documentation.
                    </p>
                </main>
                <footer class="container-fluid pt-3 mt-3 text-muted border-top">
                    <p>
                        Powered by
                        <a href="https://github.com/tfausak/monadoc">Monadoc</a>
                        version
                        <xsl:value-of select="$version"/>.
                    </p>
                </footer>
            </body>
        </html>
    </xsl:template>
</xsl:stylesheet>
