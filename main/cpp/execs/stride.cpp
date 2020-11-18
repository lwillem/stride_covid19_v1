/*
 *  This is free software: you can redistribute it and/or modify it
 *  under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  any later version.
 *  The software is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *  You should have received a copy of the GNU General Public License
 *  along with the software. If not, see <http://www.gnu.org/licenses/>.
 *
 *  Copyright 2017, 2018, Kuylen E, Willem L, Broeckhove J
 */

/**
 * @file
 * Main program: command line handling.
 */

#include "SimController.h"
#include "util/FileSys.h"
#include "util/RunConfigManager.h"
#include "util/StringUtils.h"
#include "util/TimeStamp.h"

#include <boost/property_tree/ptree.hpp>
#include <tclap/CmdLine.h>
#include <iostream>
#include <regex>
#include <string>
#include <vector>

using namespace std;
using namespace stride;
using namespace stride::util;
using namespace TCLAP;
using namespace boost::property_tree;

/// Main program of the stride simulator.
int main(int argc, char** argv)
{
        int exitStatus = EXIT_SUCCESS;

        try {
                // -----------------------------------------------------------------------------------------
                // Parse command line (parameters displayed in --help in reverse order to order below).
                // -----------------------------------------------------------------------------------------
                CmdLine cmd("stride", ' ', "2.0");


                string si = "File are read from the appropriate (config, data) directories of the "
                            "stride install directory. If false, files are read and written to the "
                            "local directory. \nDefaults to true.";
                SwitchArg installedArg("i", "installed", si, cmd, true);

                string           so = "Override configuration file parameters with values provided here.";
                MultiArg<string> overrideArg("o", "override", so, false, "<NAME>=<VALUE>", cmd);

                string sc = "Specifies the run configuration parameters. The format may be "
                            "either -c file=<file> or -c name=<name>. The first is mostly "
                            "used and may be shortened to -c <file>. The second refers to "
                            "built-in configurations specified by their name."
                            "\nDefaults to -c file=run_default.xml";
                ValueArg<string> configArg("c", "config", sc, false, "run_default.xml", "CONFIGURATION", cmd);

                vector<string>           execs{"clean", "sim"};
                ValuesConstraint<string> vc(execs);
                string                   se = "Execute the corresponding function:"
                            "  \n\t clean:  cleans configuration and writes it to a new file."
                            "  \n\t sim:    runs the simulator and is the default."
                            "\nDefaults to --exec sim.";
                ValueArg<string> execArg("e", "exec", se, false, "sim", &vc, cmd);

                cmd.parse(argc, static_cast<const char* const*>(argv));

                // -----------------------------------------------------------------------------------------
                // Get configuration and path with overrides (if any).
                // -----------------------------------------------------------------------------------------
                auto  config = configArg.getValue();
                ptree configPt;

                if (regex_search(config, regex("^name="))) {
                        config   = regex_replace(config, regex(string("^name=")), string(""));
                        configPt = RunConfigManager::Create(config);
                } else {
                        config = regex_replace(config, regex(string("^file=")), string(""));
                        const filesys::path configPath =
                            (installedArg.getValue()) ? FileSys::GetConfigDir() /= config : filesys::path(config);
                        configPt = FileSys::ReadPtreeFile(configPath);
                }

                for (const auto& p_assignment : overrideArg.getValue()) {
                        const auto v = util::Tokenize(p_assignment, "=");
                        configPt.put("run." + v[0], v[1]);
                }

                // -----------------------------------------------------------------------------------------
                // config and run simulation in cli
                // -----------------------------------------------------------------------------------------

                // add timestamp if no output prefix specified
				if (configPt.get<string>("run.output_prefix", "").empty()) {
						configPt.put("run.output_prefix", TimeStamp().ToTag().append("/"));
				}

                // sort the configuration details
				configPt.sort();

				// activate the controller
				SimController(configPt).Control();




        } catch (exception& e) {
                exitStatus = EXIT_FAILURE;
                cerr << "\nEXCEPTION THROWN: " << e.what() << endl;
        } catch (...) {
                exitStatus = EXIT_FAILURE;
                cerr << "\nEXCEPTION THROWN: Unknown exception." << endl;
        }
        return exitStatus;
}
