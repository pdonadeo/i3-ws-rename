package main

import (
	"encoding/json"
	"os"
	"path/filepath"
	"regexp"
	"strings"
)

type IconConf struct {
	Icon           string  `json:"icon"`
	WindowClass    *string `json:"window_class"`
	WindowInstance *string `json:"window_instance"`
	AppID          *string `json:"app_id"`
	Name           *string `json:"name"`
}

func readConf(fname string) ([]IconConf, error) {
	if fname == "" {
		return nil, nil
	}
	data, err := os.ReadFile(fname)
	if err != nil {
		return nil, err
	}
	var conf []IconConf
	if err := json.Unmarshal(data, &conf); err != nil {
		return nil, err
	}
	for i := range conf {
		lowerStrPtr := func(p *string) *string {
			if p == nil {
				return nil
			}
			s := strings.ToLower(*p)
			return &s
		}
		conf[i].WindowClass = lowerStrPtr(conf[i].WindowClass)
		conf[i].WindowInstance = lowerStrPtr(conf[i].WindowInstance)
		conf[i].AppID = lowerStrPtr(conf[i].AppID)
		conf[i].Name = lowerStrPtr(conf[i].Name)
	}
	return conf, nil
}

func searchClass(conf []IconConf, class_ string) *string {
	class_ = strings.ToLower(class_)
	for _, r := range conf {
		if r.WindowClass != nil && *r.WindowClass == class_ {
			icon := r.Icon
			return &icon
		}
	}
	return nil
}

func searchInstance(conf []IconConf, instance string) *string {
	instance = strings.ToLower(instance)
	for _, r := range conf {
		if r.WindowInstance != nil && *r.WindowInstance == instance {
			icon := r.Icon
			return &icon
		}
	}
	return nil
}

// searchClassInstance mirrors OCaml conf.ml: exact match on instance, or regex
// match when the configured pattern ends with "*" (using Str.regexp semantics,
// which are RE2-compatible for the patterns used in practice).
func searchClassInstance(conf []IconConf, class_, instance string) *string {
	class_ = strings.ToLower(class_)
	instance = strings.ToLower(instance)
	for _, r := range conf {
		if r.WindowClass == nil || *r.WindowClass != class_ {
			continue
		}
		if r.WindowInstance == nil {
			continue
		}
		wi := *r.WindowInstance
		var matched bool
		if strings.HasSuffix(wi, "*") {
			re, err := regexp.Compile(wi)
			matched = err == nil && re.MatchString(instance)
		} else {
			matched = wi == instance
		}
		if matched {
			icon := r.Icon
			return &icon
		}
	}
	return nil
}

func getDefaultConfFname(fname string) string {
	getenv := func(k string) string { v, _ := os.LookupEnv(k); return v }

	tryPath := func(p string) string {
		info, err := os.Stat(p)
		if err == nil && !info.IsDir() {
			return p
		}
		return ""
	}

	home := getenv("HOME")

	if xdg := getenv("XDG_CONFIG_HOME"); xdg != "" {
		if p := tryPath(filepath.Join(xdg, "sway", fname)); p != "" {
			return p
		}
	}
	if p := tryPath(filepath.Join(home, ".config", "sway", fname)); p != "" {
		return p
	}
	return ""
}
